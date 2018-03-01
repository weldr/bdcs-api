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

{-| BDCS API Compose-related types and functions
-}
module BDCS.API.Compose(ComposeInfo(..),
                        ComposeMsgAsk(..),
                        ComposeMsgResp(..),
                        compose)
  where

import           BDCS.Export(export)
import           Control.Monad.Except(runExceptT)
import           Control.Monad.Trans.Resource(runResourceT)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Database.Persist.Sql(ConnectionPool, runSqlPool)
import           System.FilePath.Posix((</>))

data ComposeInfo = ComposeInfo
  {  ciDest       :: FilePath                                   -- ^ Path to the compose artifact
  ,  ciId         :: T.Text                                     -- ^ Build UUID
  ,  ciResultsDir :: FilePath                                   -- ^ Directory containing the compose and other files
  ,  ciThings     :: [T.Text]                                   -- ^ Items to go into the compose
  ,  ciType       :: T.Text                                     -- ^ Build type (tar, etc.)
  } deriving (Eq, Show)

data ComposeMsgAsk = AskBuildsWaiting
                   | AskBuildsInProgress

data ComposeMsgResp = RespBuildsWaiting [T.Text]
                    | RespBuildsInProgress [T.Text]

compose :: FilePath -> ConnectionPool -> ComposeInfo -> IO ()
compose bdcs pool ComposeInfo{..} = do
    result <- runExceptT (runResourceT $ runSqlPool (export bdcs ciDest ciThings) pool)
    case result of
        Left _  -> TIO.writeFile (ciResultsDir </> "STATUS") "FAILED"
        Right _ -> TIO.writeFile (ciResultsDir </> "STATUS") "FINISHED"
