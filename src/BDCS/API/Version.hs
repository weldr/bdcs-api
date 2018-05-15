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
{-# LANGUAGE TemplateHaskell #-}

module BDCS.API.Version(buildVersion)
  where

import           Data.Version (showVersion)
import           Development.GitRev
import           Paths_bdcs_api(version)

buildVersion :: String
buildVersion = do
        let git_version = $(gitDescribe)
        if git_version == "UNKNOWN" then
            "v" ++ showVersion version
        else
            git_version
