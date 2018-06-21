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
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-| Implement a ContentType for 'TOML' so that POSTing text/x-toml with Servant will parse it.
    Add [TOML] to the Servant API to enable handling of it. See "BDCS.API.V0" for an example.
-}
module BDCS.API.TOMLMediaType(TOML,
                              ToTOML(..),
                              FromTOML(..))
  where

import qualified Data.ByteString.Lazy as BSL
import           Network.HTTP.Media((//))
import           Servant

-- | Used in the Servant ReqBody
data TOML

-- | Use Content-Type: text/x-toml
instance Accept TOML where
    contentType _ = "text" // "x-toml"

-- | toTOML needs to be implemented for the type being converted.
-- See "BDCS.API.Recipe" for an example.
class ToTOML a where
    toTOML :: a -> BSL.ByteString

-- | parseTOML needs to be implemented for the type being converted.
-- See "BDCS.API.Recipe" for an example.
class FromTOML a where
    parseTOML :: BSL.ByteString -> Either String a

-- | mimeRender is what Servant uses to connect its handling
-- of ContentType to the actual parsing into the destination type
instance ToTOML a => MimeRender TOML a where
    mimeRender _ = toTOML

-- | mimeUnrender is what Servant uses to connect its handling
-- of ContentType to the actual parsing into the destination type
instance FromTOML a => MimeUnrender TOML a where
    mimeUnrender _ = parseTOML
