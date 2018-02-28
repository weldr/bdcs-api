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

{-| BDCS API Server Configuration
-}
module BDCS.API.Config(ServerConfig(..))
  where

import BDCS.API.Utils(GitLock(..))
import BDCS.API.Compose(ComposeInfo)
import Control.Concurrent.STM.TQueue(TQueue)
import Database.Persist.Sql(ConnectionPool)

data ServerConfig = ServerConfig
  {  cfgRepoLock    :: GitLock                                  -- ^ Lock required for accessing recipe repo
  ,  cfgWorkQ       :: TQueue ComposeInfo                       -- ^ Worklist of composes
  ,  cfgPool        :: ConnectionPool                           -- ^ SQL connection pool for accessing MDDB
  ,  cfgBdcs        :: FilePath                                 -- ^ Location of the content store
  ,  cfgResultsDir  :: FilePath                                 -- ^ Base location for writing results
  }
