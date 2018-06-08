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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-| Error functions for use with "BDCS.API"
-}
module BDCS.API.Error(createAPIError,
                      tryIO,
                      APIResponse(..))
  where

import qualified Control.Exception as CE
import           Control.Monad.Except(ExceptT(..))
import           Control.Monad.IO.Class(liftIO)
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C8
import           Network.HTTP.Types(Header)
import           Servant hiding (Header)

-- | API Status response with possible error messages
-- used to report errors with API requests
--
-- This is converted to a JSON error response that is used in the API responses
--
-- > {
-- >     "status": false,
-- >     "errors": ["compose: Unsupported output type"]
-- >     }
-- > }
data APIResponse = APIResponse
    { arjStatus :: Bool
    , arjErrors :: [String]
    } deriving (Show, Eq)

instance FromJSON APIResponse where
  parseJSON = withObject "API Response JSON" $ \o -> do
    arjStatus <- o .: "status"
    arjErrors <- o .: "errors"
    return APIResponse{..}

instance ToJSON APIResponse where
  toJSON APIResponse{..} = object
    [ "status" .= arjStatus
    , "errors" .= arjErrors
    ]

-- | Create a 'ServantErr' with an error id and a message
--
-- [@base@]: The default 'ServantErr' response
-- [@status@]: The response status
-- [@messages@]: A list of human readable messages to include with the error
createAPIError :: ServantErr -> Bool -> [String] -> ServantErr
createAPIError base status messages = base { errBody=apiError, errHeaders=[jsonContentHdr] }
  where
    apiError :: C8.ByteString
    apiError = encode $ toJSON $ APIResponse status messages

    jsonContentHdr :: Header
    jsonContentHdr = ("Content-Type", "application/json")

-- | Convert IO Exceptions into an ExceptT.
tryIO :: IO a -> ExceptT String IO a
tryIO fn = ExceptT $ liftIO $ CE.catch (Right <$> fn)
                                       (\(e :: CE.IOException) -> return $ Left (show e))
