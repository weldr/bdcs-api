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
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module BDCS.API.V0(DbTest(..),
                   PackageInfo(..),
                   V0API,
                   v0ApiServer)
  where

import           BDCS.API.Recipe
import           BDCS.API.Utils(GitLock(..))
import           BDCS.DB
import           BDCS.Depclose(depclose)
import           BDCS.Depsolve(formulaToCNF, solveCNF)
import           BDCS.Groups(groupIdToNevra)
import           Utils.Monad(mapMaybeM)
import           BDCS.Projects(findProject, getProject)
import qualified Control.Concurrent.ReadWriteLock as RWL
import           Control.Monad.Except
import           Control.Monad.Logger(runStderrLoggingT)
import           Control.Monad.Reader
import           Data.Aeson
import           Data.List(intercalate)
import           Data.Maybe(fromJust, listToMaybe)
import           Data.String.Conversions(cs)
import qualified Data.Text as T
import           Data.Time.Calendar
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Servant.Options
import           Servant


data DbTest = DbTest
  {  dbStuff :: String
  } deriving (Generic, Eq, Show)

instance ToJSON DbTest
instance FromJSON DbTest

data PackageInfo = PackageInfo
  {  name    :: T.Text
  ,  summary :: T.Text
  } deriving (Generic, Eq, Show)

instance ToJSON PackageInfo
instance FromJSON PackageInfo

type V0API = "package"  :> Capture "package" T.Text :> Get '[JSON] PackageInfo
        :<|> "dbtest"   :> Get '[JSON] DbTest
        :<|> "recipes"  :> "list" :> Get '[JSON] [T.Text]
        :<|> "depsolve" :> Capture "package" T.Text :> Get '[JSON] [T.Text]

v0ApiServer :: GitLock -> ConnectionPool -> Server V0API
v0ApiServer repoLock pool = pkgInfoH
                       :<|> dbTestH
                       :<|> listRecipesH
                       :<|> depsolvePkgH
  where
    pkgInfoH package     = liftIO $ packageInfo pool package
    dbTestH              = liftIO $ dbTest pool
    listRecipesH         = liftIO $ listRecipes repoLock "master"
    depsolvePkgH package = liftIO $ depsolvePkg pool package

packageInfo :: ConnectionPool -> T.Text -> IO PackageInfo
packageInfo pool package = flip runSqlPersistMPool pool $ do
    mproj <- findProject package
    let proj_id = fromJust mproj
    mproject <- getProject proj_id
    let project = fromJust mproject
    liftIO $ print project
    let name = projectsName project
    let summary = projectsSummary project
    return (PackageInfo name summary)

dbTest :: ConnectionPool -> IO DbTest
dbTest pool = flip runSqlPersistMPool pool $ do
    mproj <- findProject "anaconda"
    let proj = fromJust mproj
    return (DbTest "fix this")

listRecipes :: GitLock -> T.Text -> IO [T.Text]
listRecipes repoLock branch = do
    RWL.withRead (gitRepoLock repoLock) $ do
        listBranchFiles (gitRepo repoLock) branch

depsolvePkg :: ConnectionPool -> T.Text -> IO [T.Text]
depsolvePkg pool package = do
    result <- runExceptT $ flip runSqlPool pool $ do
        formula <- depclose ["x86_64"] [package]
        solution <- solveCNF (formulaToCNF formula)
        mapMaybeM groupIdToNevra $ map fst $ filter snd solution
    case result of
        Left e            -> return []
        Right assignments -> return assignments


