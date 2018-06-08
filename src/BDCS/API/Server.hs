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
                       ServerStatus(..),
                       SocketException(..))
  where

import           BDCS.API.Compose(ComposeInfo(..), ComposeMsgAsk(..), ComposeMsgResp(..), compose)
import           BDCS.API.Config(ServerConfig(..))
import           BDCS.API.Recipes(openOrCreateRepo, commitRecipeDirectory)
import           BDCS.API.Utils(GitLock(..))
import           BDCS.API.V0(V0API, v0ApiServer)
import           BDCS.API.Version(buildVersion)
import           BDCS.DB(schemaVersion, getDbVersion)
import           Control.Concurrent.Async(Async, async, cancel, replicateConcurrently_, waitCatch)
import qualified Control.Concurrent.ReadWriteLock as RWL
import           Control.Concurrent.STM.TChan(newTChan, readTChan)
import           Control.Concurrent.STM.TMVar(TMVar, newTMVar, putTMVar, readTMVar, takeTMVar)
import           Control.Conditional(whenM)
import qualified Control.Exception as CE
import           Control.Monad(forever, void)
import           Control.Monad.Except(runExceptT)
import           Control.Monad.Logger(runFileLoggingT, runStderrLoggingT)
import           Control.Monad.STM(atomically)
import           Data.Aeson
import           Data.IORef(IORef, atomicModifyIORef', newIORef, readIORef)
import qualified Data.Map as Map
import           Data.Sequence((|>), Seq(..), deleteAt, empty, findIndexL, index)
import           Data.String.Conversions(cs)
import qualified Data.Text as T
import           Database.Persist.Sqlite
import           GHC.Conc(retry)
import           GHC.Exts(toList)
import qualified GI.Ggit as Git
import           Network.Socket
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Servant
import           System.Directory(createDirectoryIfMissing, doesPathExist, removePathForcibly)
import           System.Environment(lookupEnv)
import           System.FilePath.Posix((</>))
import           System.Posix.Files(setFileMode, setOwnerAndGroup)
import           System.Posix.User(GroupEntry(..), getGroupEntryForName)
import           Text.Read(readMaybe)

data SocketException = BadFileDescriptor
                     | BadGroup String
                     | NoSocketError
 deriving(Show)

instance CE.Exception SocketException

type InProgressMap = Map.Map T.Text (Async (), ComposeInfo)

-- | The status of the server, the database, and the API.
data ServerStatus = ServerStatus
  {  srvApi           :: String                                 -- ^ Supported API version
  ,  srvBackend       :: String                                 -- ^ Backend implementation (weldr, lorax-composer)
  ,  srvBuild         :: String                                 -- ^ Server build version
  ,  srvSchemaVersion :: String                                 -- ^ Supported Database Schema version
  ,  srvDbVersion     :: String                                 -- ^ Database version
  ,  srvDbSupported   :: Bool                                   -- ^ True if the Database is supported by the Server
  } deriving (Eq, Show)

instance ToJSON ServerStatus where
  toJSON ServerStatus{..} = object
    [ "api"            .= srvApi
    , "backend"        .= srvBackend
    , "build"          .= srvBuild
    , "schema_version" .= srvSchemaVersion
    , "db_version"     .= srvDbVersion
    , "db_supported"   .= srvDbSupported ]

instance FromJSON ServerStatus where
  parseJSON = withObject "server status" $ \o -> do
    srvApi           <- o .: "api"
    srvBackend       <- o .: "backend"
    srvBuild         <- o .: "build"
    srvSchemaVersion <- o .: "schema_version"
    srvDbVersion     <- o .: "db_version"
    srvDbSupported   <- o .: "db_supported"
    return ServerStatus{..}

-- | The /status route
type CommonAPI = "api" :> "status" :> Get '[JSON] ServerStatus

-- The maximum number of composes that can run simultaneously.  Modify this for your site's
-- requirements and capabilities.
maxComposes :: Int
maxComposes = 1

serverStatus :: ServerConfig -> Handler ServerStatus
serverStatus ServerConfig{..} = do
    version <- dbVersion
    return (ServerStatus "0" "weldr" buildVersion (show schemaVersion) (show version) (schemaVersion == version))
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
             , corsMethods = "DELETE" : "PUT" : simpleMethods }

-- | Servant 'Proxy'
--
-- This connects the API to everything else
proxyAPI :: Proxy CombinedAPI
proxyAPI = Proxy

application :: ServerConfig -> Application
application cfg = appCors
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

    chan <- atomically newTChan

    let cfg = ServerConfig { cfgRepoLock = GitLock lock repo,
                             cfgChan = chan,
                             cfgPool = pool,
                             cfgBdcs = bdcsPath,
                             cfgResultsDir = "/var/lib/composer" }

    createDirectoryIfMissing True (cfgResultsDir cfg)

    -- Fork off another process that does the composes in the background,
    -- which means the client immediately gets a response with a build ID.
    -- The compose (which could take a while) proceeds independently.  The
    -- client uses a different route to check and fetch the results.
    void $ async $ composeServer cfg

    return $ application cfg

-- | Run the API server
runServer :: FilePath -> String -> FilePath -> FilePath -> FilePath -> IO ()
runServer socketPath socketGroup bdcsPath gitRepoPath sqliteDbPath = void $ withSocketsDo $ do
    sock <- getSocket socketPath
    app  <- mkApp bdcsPath gitRepoPath sqliteDbPath
    runSettingsSocket defaultSettings sock app
 where
    getSocket :: FilePath -> IO Socket
    getSocket fp = lookupEnv "LISTEN_FDS" >>= \case
        Nothing -> if fp == "" then CE.throw NoSocketError else newSocket fp
        Just s  -> case readMaybe s of
            Nothing -> CE.throw BadFileDescriptor
            Just fd -> mkSocket fd AF_UNIX Stream defaultProtocol Bound

    newSocket :: FilePath -> IO Socket
    newSocket path = do
        whenM (doesPathExist path) $
            removePathForcibly path

        gid <- CE.catch (groupID <$> getGroupEntryForName socketGroup)
                        (\(_ :: CE.IOException) -> CE.throw $ BadGroup socketGroup)

        s <- socket AF_UNIX Stream defaultProtocol
        bind s (SockAddrUnix path)
        listen s 1
        setFileMode path 0o660
        setOwnerAndGroup path 0 gid
        return s

composeServer :: ServerConfig -> IO ()
composeServer ServerConfig{..} = do
    -- A mutable variable that lets us keep track about currently running composes.
    -- This is a map from UUID of the compose underway to the ThreadId doing that
    -- compose.  This lets us kill threads if needed.  If this is empty, no compose
    -- is currently running.
    inProgressRef <- newIORef Map.empty

    -- A list of all composes currently waiting to be run.
    worklist <- atomically $ newTMVar empty

    -- From here, we run several separate threads forever.
    --
    -- One thread reads messages out of the channel and responds to them.  This includes
    -- things like "what is waiting in the queue?" and "what is currently composing?".
    -- It also includes requests to start new composes.
    --
    -- All the other threads are worker threads that run composes.  We run as many threads
    -- as we are allowed maximum simultaneous composes.  Each thread does one compose at
    -- a time - reading the first item out of the worklist, starting the compose, and
    -- waiting for it to finish.  When one compose is finished, it can look at the list to
    -- see about starting the next one.
    void $ async $ messagesThread inProgressRef worklist
    replicateConcurrently_ maxComposes (workerThread inProgressRef worklist)
 where
    -- Add a newly started compose to the in progress map.
    addCompose :: IORef InProgressMap -> ComposeInfo -> Async () -> IO ()
    addCompose ref ci@ComposeInfo{..} thread =
        void $ atomicModifyIORef' ref (\m -> (Map.insert ciId (thread, ci) m, ()))

    -- Remove a completed (or killed?) compose from the in progress map.
    removeCompose :: IORef InProgressMap -> T.Text -> IO ()
    removeCompose ref uuid =
        void $ atomicModifyIORef' ref (\m -> (Map.delete uuid m, ()))

    workerThread :: IORef InProgressMap -> TMVar (Seq ComposeInfo) -> IO ()
    workerThread inProgressRef worklist = forever $ do
        -- Attempt to grab the first ComposeInfo out of the worklist.  This call blocks the
        -- worker thread until something appears in the list and we can get it.
        ci <- atomically $ takeTMVar worklist >>= \case
            (x :<| xs) -> putTMVar worklist xs >> return x
            -- This retry call is critical - without it, the worker threads and messages
            -- thread will deadlock trying to read the worklist.
            _          -> retry

        -- We got a ComposeInfo.  Start the compose in a separate thread and wait
        -- for it to finish (which could be due to success, failure, or cancellation).
        thread <- async $ runFileLoggingT (ciResultsDir ci </> "compose.log")
                                          (compose cfgBdcs cfgPool ci)

        addCompose inProgressRef ci thread
        void $ waitCatch thread
        removeCompose inProgressRef (ciId ci)

    messagesThread :: IORef InProgressMap -> TMVar (Seq ComposeInfo) -> IO ()
    messagesThread inProgressRef worklist = forever $ atomically (readTChan cfgChan) >>= \case
        (AskBuildsWaiting, Just r) -> do
            lst <- atomically $ readTMVar worklist
            atomically $ putTMVar r (RespBuildsWaiting $ map ciId (toList lst))

        (AskBuildsInProgress, Just r) -> do
            -- Get just the ComposeInfo records for all the in-progress composes.
            inProgress <- map snd . Map.elems <$> readIORef inProgressRef
            -- And then extract the UUIDs of each, and that's the answer.
            atomically $ putTMVar r (RespBuildsInProgress $ map ciId inProgress)

        (AskCancelBuild buildId, Just r) -> do
            inProgress <- readIORef inProgressRef
            case Map.lookup buildId inProgress of
                Just (thread, ci) -> do cancel thread
                                        removeCompose inProgressRef buildId
                                        removePathForcibly (ciResultsDir ci)
                                        atomically $ putTMVar r (RespBuildCancelled True)

                _                 -> atomically $ putTMVar r (RespBuildCancelled False)

        (AskCompose ci, _) -> atomically $ do
            -- Add the new compose to the end of the work queue.  It will eventually
            -- get around to being run by composesThread.
            lst <- takeTMVar worklist
            putTMVar worklist (lst |> ci)

        (AskDequeueBuild buildId, Just r) -> do
            -- The worklist stores ComposeInfo records, but we only get the UUID from the
            -- client.  So first we have to find the right element in the worklist.  Some
            -- element with that UUID should be present, but we can't guarantee that given
            -- all the multiprocessing stuff.  Hence the Maybe.
            ci <- atomically $ do
                lst <- takeTMVar worklist
                case findIndexL (\e -> ciId e == buildId) lst of
                    Nothing  -> return Nothing
                    Just ndx -> do let ele = index lst ndx
                                   putTMVar worklist (deleteAt ndx lst)
                                   return $ Just ele

            -- If we found a ComposeInfo, clean it up - remove the results directory
            -- (that doesn't yet have an artifact, but should have some toml files) and
            -- inform the client.  We already removed it from the worklist in the block
            -- above.
            case ci of
                Just ComposeInfo{..} -> do
                    removePathForcibly ciResultsDir
                    atomically $ putTMVar r (RespBuildDequeued True)

                Nothing -> atomically $ putTMVar r (RespBuildDequeued False)

        _ -> return ()
