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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| BDCS API Compose-related types and functions
-}
module BDCS.API.Compose(ComposeInfo(..),
                        ComposeMsgAsk(..),
                        ComposeMsgResp(..),
                        ComposeStatus(..),
                        compose,
                        mkComposeStatus)
  where

import           BDCS.API.Recipe(Recipe(..), parseRecipe)
import           BDCS.Export(export)
import qualified Control.Exception as CE
import           Control.Monad.Except(ExceptT(..), runExceptT)
import           Control.Monad.IO.Class(liftIO)
import           Control.Monad.Trans.Resource(runResourceT)
import           Data.Aeson((.:), (.=), FromJSON(..), ToJSON(..), object, withObject)
import           Data.String.Conversions(cs)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Database.Persist.Sql(ConnectionPool, runSqlPool)
import           System.FilePath.Posix((</>))
import           System.Posix.Files(getFileStatus, modificationTime)
import           System.Posix.Types(EpochTime)

data ComposeInfo = ComposeInfo
  {  ciDest       :: FilePath                                   -- ^ Path to the compose artifact
  ,  ciId         :: T.Text                                     -- ^ Build UUID
  ,  ciResultsDir :: FilePath                                   -- ^ Directory containing the compose and other files
  ,  ciThings     :: [T.Text]                                   -- ^ Items to go into the compose
  ,  ciType       :: T.Text                                     -- ^ Build type (tar, etc.)
  } deriving (Eq, Show)

data ComposeStatus = ComposeStatus {
    csBuildId       :: T.Text,
    csName          :: T.Text,
    csQueueStatus   :: T.Text,
    csTimestamp     :: EpochTime,
    csVersion       :: T.Text
} deriving (Show, Eq)

instance ToJSON ComposeStatus where
    toJSON ComposeStatus{..} = object [
        "id"            .= csBuildId
      , "recipe"        .= csName
      , "queue_status"  .= csQueueStatus
      , "timestamp"     .= csTimestamp
      , "version"       .= csVersion ]

instance FromJSON ComposeStatus where
    parseJSON = withObject "compose type" $ \o ->
        ComposeStatus <$> o .: "id"
                      <*> o .: "recipe"
                      <*> o .: "queue_status"
                      <*> o .: "timestamp"
                      <*> o .: "version"

data ComposeMsgAsk = AskBuildsWaiting
                   | AskBuildsInProgress

data ComposeMsgResp = RespBuildsWaiting [T.Text]
                    | RespBuildsInProgress [T.Text]

compose :: FilePath -> ConnectionPool -> ComposeInfo -> IO ()
compose bdcs pool ComposeInfo{..} = do
    TIO.writeFile (ciResultsDir </> "STATUS") "RUNNING"

    result <- runExceptT (runResourceT $ runSqlPool (export bdcs ciDest ciThings) pool)
    case result of
        Left _  -> TIO.writeFile (ciResultsDir </> "STATUS") "FAILED"
        Right _ -> TIO.writeFile (ciResultsDir </> "STATUS") "FINISHED"

mkComposeStatus :: FilePath -> T.Text -> ExceptT String IO ComposeStatus
mkComposeStatus baseDir buildId = do
    let path = baseDir </> cs buildId

    contents   <- tryIO   $ TIO.readFile (path </> "recipe.toml")
    Recipe{..} <- ExceptT $ return $ parseRecipe contents
    mtime      <- tryIO   $ modificationTime <$> getFileStatus (path </> "STATUS")
    status     <- tryIO   $ TIO.readFile (path </> "STATUS")

    return ComposeStatus { csBuildId = buildId,
                           csName = cs rName,
                           csQueueStatus = status,
                           csTimestamp = mtime,
                           csVersion = maybe "0.0.1" cs rVersion }
 where
     tryIO :: IO a -> ExceptT String IO a
     tryIO fn = ExceptT $ liftIO $ CE.catch (Right <$> fn)
                                            (\(e :: CE.IOException) -> return $ Left (show e))
