-- Copyright (C) 2017 Red Hat, Inc.
--
-- This file is part of bdcs-api.
--
-- bdcs-api is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- bdcs-api is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with bdcs-api.  If not, see <http://www.gnu.org/licenses/>.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

{-| BDCS API Server

    This starts a server and answers the API requests.
-}
module BDCS.API.Server(mkApp,
                       proxyAPI,
                       runServer,
                       ServerStatus(..))
  where

import           BDCS.API.Compose(ComposeInfo(..), ComposeMsgAsk(..), ComposeMsgResp(..), compose)
import           BDCS.API.Config(AskTuple, ServerConfig(..))
import           BDCS.API.Recipes(openOrCreateRepo, commitRecipeDirectory)
import           BDCS.API.Utils(GitLock(..))
import           BDCS.API.V0(V0API, v0ApiServer)
import           BDCS.API.Version(apiVersion)
import           BDCS.DB(schemaVersion, getDbVersion)
import           Control.Concurrent(ThreadId, forkFinally, forkIO)
import qualified Control.Concurrent.ReadWriteLock as RWL
import           Control.Concurrent.STM.TChan(newTChan, tryReadTChan)
import           Control.Concurrent.STM.TMVar(putTMVar)
import           Control.Monad(forever, void, when)
import           Control.Monad.Except(runExceptT)
import           Control.Monad.Logger(runStderrLoggingT)
import           Control.Monad.STM(atomically)
import           Data.Aeson
import           Data.Int(Int64)
import           Data.IORef(IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map as Map
import           Data.String.Conversions(cs)
import qualified Data.Text as T
import           Database.Persist.Sqlite
import qualified GI.Ggit as Git
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Servant.Options
import           Servant
import           System.Directory(createDirectoryIfMissing)

type InProgressMap = Map.Map T.Text (ThreadId, ComposeInfo)

-- | The status of the server, the database, and the API.
data ServerStatus = ServerStatus
  {  srvBackend   :: String                                     -- ^ Backend implementation (weldr, lorax-composer)
  ,  srvVersion   :: String                                     -- ^ Server version
  ,  srvSchema    :: Int64                                      -- ^ Supported Database Schema version
  ,  srvDb        :: Int64                                      -- ^ Database version
  ,  srvSupported :: Bool                                       -- ^ True if the Database is supported by the Server
  } deriving (Eq, Show)

instance ToJSON ServerStatus where
  toJSON ServerStatus{..} = object
    [ "backend"   .= srvBackend
    , "version"   .= srvVersion
    , "schema"    .= srvSchema
    , "db"        .= srvDb
    , "supported" .= srvSupported ]

instance FromJSON ServerStatus where
  parseJSON = withObject "server status" $ \o -> do
    srvBackend   <- o .: "backend"
    srvVersion   <- o .: "version"
    srvSchema    <- o .: "schema"
    srvDb        <- o .: "db"
    srvSupported <- o .: "supported"
    return ServerStatus{..}

-- | The /status route
type CommonAPI = "api" :> "status" :> Get '[JSON] ServerStatus


serverStatus :: ServerConfig -> Handler ServerStatus
serverStatus ServerConfig{..} = do
    version <- dbVersion
    return (ServerStatus "weldr" apiVersion schemaVersion version (schemaVersion == version))
  where
    dbVersion = do
        result <- runExceptT $ runSqlPool getDbVersion cfgPool
        case result of
            Left _        -> return 0
            Right version -> return version

commonServer :: ServerConfig -> Server CommonAPI
commonServer cfg = serverStatus cfg

-- | The combined API routes, /status and /api/v0/*
type CombinedAPI = CommonAPI
              :<|> "api" :> "v0" :> V0API

combinedServer :: ServerConfig -> Server CombinedAPI
combinedServer cfg = commonServer cfg
                :<|> v0ApiServer cfg

-- | CORS policy
appCors :: Middleware
appCors = cors (const $ Just policy)
  where
    policy = simpleCorsResourcePolicy
             { corsRequestHeaders = ["Content-Type"]
             , corsMethods = "PUT" : simpleMethods }

-- | Servant 'Proxy'
--
-- This connects the API to everything else
proxyAPI :: Proxy CombinedAPI
proxyAPI = Proxy

app :: ServerConfig -> Application
app cfg = appCors
        $ provideOptions proxyAPI
        $ serve proxyAPI
        $ combinedServer cfg

-- | Create the server app
--
-- Create a SQLite connection pool, open/create the Git repo, and return the app
mkApp :: FilePath -> FilePath -> FilePath -> IO Application
mkApp bdcsPath gitRepoPath sqliteDbPath = do
    pool <- runStderrLoggingT $ createSqlitePool (cs sqliteDbPath) 5
--    runSqlPool (runMigration migrateAll) pool

    Git.init
    repo <- openOrCreateRepo gitRepoPath
    void $ commitRecipeDirectory repo "master" gitRepoPath
    lock <- RWL.new

    q    <- newIORef []
    chan <- atomically newTChan

    let cfg = ServerConfig { cfgRepoLock = GitLock lock repo,
                             cfgChan = chan,
                             cfgWorkQ = q,
                             cfgPool = pool,
                             cfgBdcs = bdcsPath,
                             cfgResultsDir = "/var/lib/composer" }

    createDirectoryIfMissing True (cfgResultsDir cfg)

    -- Fork off another process that does the composes in the background,
    -- which means the client immediately gets a response with a build ID.
    -- The compose (which could take a while) proceeds independently.  The
    -- client uses a different route to check and fetch the results.
    void $ forkIO $ composeServer cfg

    return $ app cfg

-- | Run the API server
runServer :: Int -> FilePath -> FilePath -> FilePath -> IO ()
runServer port bdcsPath gitRepoPath sqliteDbPath = run port =<< mkApp bdcsPath gitRepoPath sqliteDbPath

composeServer :: ServerConfig -> IO ()
composeServer ServerConfig{..} = do
    -- A mutable variable that lets us keep track about currently running composes.
    -- This is a map from UUID of the compose underway to the ThreadId doing that
    -- compose.  This lets us kill threads if needed.  If this is empty, no compose
    -- is currently running.
    inProgressRef <- newIORef Map.empty

    -- We run in a loop forever doing two tasks:
    --
    -- (1) Reading questions out of the shared one-way communications channel and
    -- sending responses back to the API server.
    --
    -- (2) Firing off composes for requests in the queue.
    forever $ do
        atomically (tryReadTChan cfgChan) >>= respondToMessage inProgressRef

        -- For now, we only support running one compose at a time.  If the mutable
        -- variable is not empty, we are already running a compose.  Don't try to start
        -- another one.  This leaves the work queue alone so we can grab the next item
        -- later.
        inProgress <- readIORef inProgressRef
        when (Map.null inProgress) $
            nextInQ cfgWorkQ >>= \case
                -- Start another compose.  When the thread finishes (either because the
                -- compose is done or because it failed), clear out the mutable variable.
                Just ci -> do threadId <- forkFinally (compose cfgBdcs cfgPool ci)
                                                      (\_ -> removeCompose inProgressRef (ciId ci))

                              addCompose inProgressRef ci threadId

                _ -> return ()
 where
    -- Pop the next element off the head of the queue and return it.
    nextInQ :: IORef [a] -> IO (Maybe a)
    nextInQ q = atomicModifyIORef' q (\case (hd:tl) -> (tl, Just hd)
                                            _       -> ([], Nothing))

    -- Add a newly started compose to the in progress map.
    addCompose :: IORef InProgressMap -> ComposeInfo -> ThreadId -> IO ()
    addCompose ref ci@ComposeInfo{..} threadId =
        void $ atomicModifyIORef' ref (\m -> (Map.insert ciId (threadId, ci) m, ()))

    -- Remove a completed (or killed?) compose from the in progress map.
    removeCompose :: IORef InProgressMap -> T.Text -> IO ()
    removeCompose ref uuid =
        void $ atomicModifyIORef' ref (\m -> (Map.delete uuid m, ()))

    respondToMessage :: IORef InProgressMap -> Maybe AskTuple -> IO ()
    respondToMessage _ (Just (AskBuildsWaiting, r)) = do
        q <- readIORef cfgWorkQ
        atomically $ putTMVar r (RespBuildsWaiting $ map ciId q)

    respondToMessage ref (Just (AskBuildsInProgress, r)) = do
        -- Get just the ComposeInfo records for all the in-progress composes.
        inProgress <- map snd . Map.elems <$> readIORef ref
        -- And then extract the UUIDs of each, and that's the answer.
        atomically $ putTMVar r (RespBuildsInProgress $ map ciId inProgress)

    respondToMessage _ _ = return ()
