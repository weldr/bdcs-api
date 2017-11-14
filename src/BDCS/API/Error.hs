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

module BDCS.API.Error(createApiError)
  where

import qualified Data.ByteString.Lazy.Char8 as C8
import           Network.HTTP.Types(Header)
import           Servant hiding (Header)

createApiError :: ServantErr -> String -> String -> ServantErr
createApiError base apiId message = base { errBody=apiError, errHeaders=[jsonContentHdr] }
  where
    apiError :: C8.ByteString
    apiError = C8.pack $ "{ \"id\":\"" ++ apiId ++ "\", \"message\":\"" ++ message ++ "\" }"

    jsonContentHdr :: Header
    jsonContentHdr = ("Content-Type", "application/json")
