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
module BDCS.API.Config(AskTuple,
                       ServerConfig(..))
  where

import BDCS.API.Utils(GitLock(..))
import BDCS.API.Compose(ComposeMsgAsk, ComposeMsgResp)
import Control.Concurrent.STM.TChan(TChan)
import Control.Concurrent.STM.TMVar(TMVar)
import Database.Persist.Sql(ConnectionPool)

type AskTuple = (ComposeMsgAsk, Maybe (TMVar ComposeMsgResp))

data ServerConfig = ServerConfig
  {  cfgRepoLock    :: GitLock                                  -- ^ Lock required for accessing recipe repo
  ,  cfgChan        :: TChan AskTuple                           -- ^ Channel for the API server to ask things of
                                                                -- the compose server.  The tuple is the message
                                                                -- that needs a response and a location for where
                                                                -- the response should be written to.
  ,  cfgPool        :: ConnectionPool                           -- ^ SQL connection pool for accessing MDDB
  ,  cfgBdcs        :: FilePath                                 -- ^ Location of the content store
  ,  cfgResultsDir  :: FilePath                                 -- ^ Base location for writing results
  }
