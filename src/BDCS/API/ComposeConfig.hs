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
{-# LANGUAGE RecordWildCards   #-}

module BDCS.API.ComposeConfig(
    ComposeConfig(..),

    parseComposeConfig,
    composeConfigTOML
) where

import           BDCS.Export.Types(ExportType(..), exportTypeFromText, exportTypeText)
import           Data.Aeson
import           Data.Maybe(fromMaybe)
import qualified Data.Text as T
import           Text.Printf(printf)
import           Text.Toml(parseTomlDoc)


-- | Information about the compose configuration not available in other results files
data ComposeConfig = ComposeConfig {
    ccCommit       :: T.Text,                                           -- ^ Commit hash for Blueprint
    ccExportType   :: ExportType                                        -- ^ Export type
} deriving (Show, Eq)

instance ToJSON ComposeConfig where
  toJSON ComposeConfig{..} = object
    [ "commit"      .= ccCommit
    , "export_type" .= exportTypeText ccExportType
    ]

instance FromJSON ComposeConfig where
  parseJSON = withObject "Compose configuration data" $ \o -> do
    ccCommit     <- o .: "commit"
    ccExportType <- (o .: "export_type") >>= \et -> return $ fromMaybe ExportTar $ exportTypeFromText et
    return ComposeConfig{..}

-- | Parse a TOML formatted compose config string and return a ComposeConfig
--
-- If there is an error the details will be returned in the Left
parseComposeConfig :: T.Text -> Either String ComposeConfig
parseComposeConfig xs =
    case parseTomlDoc "" xs of
        Left err    -> Left ("Parsing TOML document failed. " ++ show err)
        Right table -> do
            let jsonValue = toJSON table
            case (fromJSON jsonValue :: Result ComposeConfig) of
                Error err -> Left ("Converting from JSON to ComposeConfig failed. " ++ show err)
                Success r -> Right r

-- | Return a TOML string from a ComposeConfig record
composeConfigTOML :: ComposeConfig -> T.Text
composeConfigTOML ComposeConfig{..} = T.concat [commitText, exportText]
  where
    commitText = T.pack $ printf "commit = \"%s\"\n" ccCommit
    exportText = T.pack $ printf "export_type = \"%s\"\n" (exportTypeText ccExportType)
