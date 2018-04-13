-- Copyright (C) 2018 Red Hat, Inc.
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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| BDCS API Compose-related types and functions
-}
module BDCS.API.Compose(ComposeInfo(..),
                        ComposeMsgAsk(..),
                        ComposeMsgResp(..),
                        ComposeStatus(..),
                        UuidError(..),
                        UuidStatus(..),
                        compose,
                        deleteCompose,
                        getComposesWithStatus,
                        mkComposeStatus)
  where

import           BDCS.API.Depsolve(PackageNEVRA(..), depsolveRecipe)
import           BDCS.API.QueueStatus(QueueStatus(..), queueStatusEnded, queueStatusText, queueStatusFromText)
import           BDCS.API.Recipe(Recipe(..), parseRecipe)
import           BDCS.Export(exportAndCustomize)
import           BDCS.Export.Customize(Customization)
import           BDCS.Export.Types(ExportType)
import           BDCS.Utils.Either(maybeToEither)
import           Control.Conditional(ifM)
import qualified Control.Exception as CE
import           Control.Monad(filterM)
import           Control.Monad.Except(ExceptT(..), runExceptT)
import           Control.Monad.Logger(MonadLoggerIO, logErrorN, logInfoN)
import           Control.Monad.IO.Class(liftIO)
import           Control.Monad.Trans.Resource(MonadBaseControl, MonadThrow, runResourceT)
import           Data.Aeson((.:), (.=), FromJSON(..), ToJSON(..), object, withObject)
import           Data.Time.Clock(UTCTime, getCurrentTime)
import           Data.Either(rights)
import           Data.String.Conversions(cs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Database.Persist.Sql(ConnectionPool, runSqlPool)
import           System.Directory(doesFileExist, getModificationTime, listDirectory, removePathForcibly)
import           System.FilePath.Posix((</>))

data ComposeInfo = ComposeInfo
  {  ciDest       :: FilePath                                   -- ^ Path to the compose artifact
  ,  ciId         :: T.Text                                     -- ^ Build UUID
  ,  ciRecipe     :: Recipe                                     -- ^ The recipe being built
  ,  ciResultsDir :: FilePath                                   -- ^ Directory containing the compose and other files
  ,  ciCustom     :: [Customization]                            -- ^ Customizations to perform on the items in the compose
  ,  ciType       :: ExportType                                 -- ^ Build type (tar, etc.)
  } deriving (Eq, Show)

data ComposeStatus = ComposeStatus {
    csBuildId       :: T.Text,
    csName          :: T.Text,
    csQueueStatus   :: QueueStatus,
    csTimestamp     :: UTCTime,
    csVersion       :: T.Text
} deriving (Show, Eq)

instance ToJSON ComposeStatus where
    toJSON ComposeStatus{..} = object [
        "id"            .= csBuildId
      , "blueprint"     .= csName
      , "queue_status"  .= csQueueStatus
      , "timestamp"     .= csTimestamp
      , "version"       .= csVersion ]

instance FromJSON ComposeStatus where
    parseJSON = withObject "compose type" $ \o ->
        ComposeStatus <$> o .: "id"
                      <*> o .: "blueprint"
                      <*> o .: "queue_status"
                      <*> o .: "timestamp"
                      <*> o .: "version"

data UuidError = UuidError {
    ueError :: T.Text,
    ueUuid :: T.Text
} deriving (Show, Eq)

instance ToJSON UuidError where
    toJSON UuidError{..} = object [
        "error" .= ueError,
        "uuid"  .= ueUuid ]

instance FromJSON UuidError where
    parseJSON = withObject "UUID error type" $ \o ->
        UuidError <$> o .: "error"
                  <*> o .: "uuid"

data UuidStatus = UuidStatus {
    usStatus :: Bool,
    usUuid :: T.Text
} deriving (Show, Eq)

instance ToJSON UuidStatus where
    toJSON UuidStatus{..} = object [
        "status" .= usStatus,
        "uuid"   .= usUuid ]

instance FromJSON UuidStatus where
    parseJSON = withObject "UUID type" $ \o ->
        UuidStatus <$> o .: "status"
                   <*> o .: "uuid"

data ComposeMsgAsk = AskBuildsWaiting
                   | AskBuildsInProgress

data ComposeMsgResp = RespBuildsWaiting [T.Text]
                    | RespBuildsInProgress [T.Text]

compose :: (MonadBaseControl IO m, MonadLoggerIO m, MonadThrow m) => FilePath -> ConnectionPool -> ComposeInfo -> m ()
compose bdcs pool ComposeInfo{..} = do
    logStatus QRunning "Compose started on"

    depsolveRecipe pool ciRecipe >>= \case
        Left e            -> logErrorN (cs e) >> logStatus QFailed "Compose failed on"
        Right (nevras, _) -> do let things = map pkgString nevras
                                logInfoN $ "Exporting packages: " `T.append` T.intercalate " " things

                                runExceptT (runResourceT $ runSqlPool (exportAndCustomize bdcs ciDest ciType things ciCustom) pool) >>= \case
                                    Left e  -> logErrorN (cs e) >> logStatus QFailed "Compose failed on"
                                    Right _ -> logStatus QFinished "Compose finished on"
 where
    pkgString :: PackageNEVRA -> T.Text
    pkgString PackageNEVRA{..} = T.concat [pnName, "-", pnVersion, "-", pnRelease, ".", pnArch]

    logStatus :: MonadLoggerIO m => QueueStatus -> T.Text -> m ()
    logStatus status msg = do
        time <- liftIO $ do
            TIO.writeFile (ciResultsDir </> "STATUS") (queueStatusText status)
            getCurrentTime

        logInfoN $ T.concat [msg, " ", cs (show time)]

deleteCompose :: FilePath -> T.Text -> IO (Either UuidError UuidStatus)
deleteCompose dir uuid =
    liftIO (runExceptT $ mkComposeStatus dir uuid) >>= \case
        Left _                  -> return $ Left UuidError { ueError="Not a valid build uuid", ueUuid=uuid }
        Right ComposeStatus{..} ->
            if not (queueStatusEnded csQueueStatus)
            then return $ Left UuidError { ueError="Build not in FINISHED or FAILED", ueUuid=uuid }
            else do
                let path = dir </> cs uuid
                CE.catch (do removePathForcibly path
                             return $ Right UuidStatus { usStatus=True, usUuid=uuid })
                         (\(e :: CE.IOException) -> return $ Left UuidError { ueError=cs $ show e, ueUuid=uuid })

getComposesWithStatus :: FilePath -> QueueStatus -> IO [ComposeStatus]
getComposesWithStatus resultsDir status = do
    -- First, gather up all the subdirectories of resultsDir.  Each of these is a UUID for
    -- some compose, wher that one has finished or is in progress or is in the queue.
    contents <- listDirectory resultsDir
    -- Next, filter that list down to just those that have a STATUS file containing the
    -- sought after status.
    uuids    <- filterM matches (map cs contents)
    -- Finally, convert those into ComposeStatus records.
    rights <$> mapM (runExceptT . mkComposeStatus resultsDir) uuids
 where
    matches :: T.Text -> IO Bool
    matches uuid = do
        let statusFile = resultsDir </> cs uuid </> "STATUS"
        ifM (doesFileExist statusFile)
            (do line <- CE.catch (TIO.readFile statusFile)
                                 (\(_ :: CE.IOException) -> return "")
                return $ queueStatusFromText line == Just status)
            (return False)

mkComposeStatus :: FilePath -> T.Text -> ExceptT String IO ComposeStatus
mkComposeStatus baseDir buildId = do
    let path = baseDir </> cs buildId

    contents   <- tryIO   $ TIO.readFile (path </> "blueprint.toml")
    Recipe{..} <- ExceptT $ return $ parseRecipe contents
    mtime      <- tryIO   $ getModificationTime (path </> "STATUS")
    status     <- tryIO   $ TIO.readFile (path </> "STATUS")

    status'    <- maybeToEither "Unknown queue status for compose" (queueStatusFromText status)

    return ComposeStatus { csBuildId = buildId,
                           csName = cs rName,
                           csQueueStatus = status',
                           csTimestamp = mtime,
                           csVersion = maybe "0.0.1" cs rVersion }
 where
     tryIO :: IO a -> ExceptT String IO a
     tryIO fn = ExceptT $ liftIO $ CE.catch (Right <$> fn)
                                            (\(e :: CE.IOException) -> return $ Left (show e))
