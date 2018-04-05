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

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module BDCS.API.Depsolve(PackageNEVRA(..),
                         mkPackageNEVRA,
                         depsolveProjects,
                         depsolveRecipe)
 where

import           BDCS.Depclose(depcloseNames)
import           BDCS.Depsolve(formulaToCNF, solveCNF)
import           BDCS.Groups(groupIdToNevra)
import           BDCS.RPM.Utils(splitFilename)
import           BDCS.Utils.Monad(mapMaybeM)
import           Control.Monad.Except(runExceptT)
import           Control.Monad.IO.Class(MonadIO)
import           Control.Monad.Trans.Resource(MonadBaseControl)
import           Data.Aeson((.=), (.:), FromJSON(..), ToJSON(..), object, withObject)
import           Data.List(find)
import           Data.Maybe(fromMaybe, mapMaybe)
import           Data.String.Conversions(cs)
import qualified Data.Text as T
import           Database.Persist.Sql(ConnectionPool, runSqlPool)

import BDCS.API.Recipe(Recipe(..), getAllRecipeProjects)

-- | Package build details
data PackageNEVRA = PackageNEVRA {
     pnName       :: T.Text
   , pnEpoch      :: Maybe Int
   , pnVersion    :: T.Text
   , pnRelease    :: T.Text
   , pnArch       :: T.Text
} deriving (Show, Eq)

instance ToJSON PackageNEVRA where
  toJSON PackageNEVRA{..} = object [
        "name"    .= pnName
      , "epoch"   .= fromMaybe 0 pnEpoch
      , "version" .= pnVersion
      , "release" .= pnRelease
      , "arch"    .= pnArch ]

instance FromJSON PackageNEVRA where
  parseJSON = withObject "package NEVRA" $ \o ->
      PackageNEVRA <$> o .: "name"
                   <*> o .: "epoch"
                   <*> o .: "version"
                   <*> o .: "release"
                   <*> o .: "arch"

-- Make a PackageNEVRA from a tuple of NEVRA info.
mkPackageNEVRA :: (T.Text, Maybe T.Text, T.Text, T.Text, T.Text) -> PackageNEVRA
mkPackageNEVRA (name, epoch, version, release, arch) = PackageNEVRA name (epoch' epoch) version release arch                
 where                       
    epoch' Nothing = Nothing  
    epoch' (Just e) = Just ((read $ T.unpack e) :: Int)      

-- | Depsolve a list of project names, returning a list of PackageNEVRA
-- If there is an error it returns an empty list
depsolveProjects :: (MonadBaseControl IO m, MonadIO m) => ConnectionPool -> [T.Text] -> m (Either String [PackageNEVRA])
depsolveProjects pool project_name_list = do
    result <- runExceptT $ flip runSqlPool pool $ do
        -- XXX Need to properly deal with arches
        formula <- depcloseNames ["x86_64"] project_name_list
        solution <- solveCNF (formulaToCNF formula)
        mapMaybeM groupIdToNevra $ map fst $ filter snd solution
    case result of
        Left  e           -> return $ Left (show e)
        Right assignments -> return $ Right (map (mkPackageNEVRA . splitFilename) assignments)

depsolveRecipe :: (MonadBaseControl IO m, MonadIO m) => ConnectionPool -> Recipe -> m (Either T.Text ([PackageNEVRA], [PackageNEVRA]))
depsolveRecipe pool recipe@Recipe{..} = do
    -- Make a list of the packages and modules (a set) and sort it by lowercase names
    let projects_name_list = map cs $ getAllRecipeProjects recipe
    -- depsolve this list
    depsolveProjects pool projects_name_list >>= \case
        Left err         -> return $ Left (cs err)
        Right dep_nevras -> do
            -- Make a list of the NEVRAs for the names in the step above (frozen list of packages)
            -- NOTE It may not include everything, if the dependency is satisfied by a project with
            --      a different name it will not be included in the list.
            let project_nevras = getProjectNEVRAs projects_name_list dep_nevras
            return $ Right (dep_nevras, project_nevras)
 where
    -- Get the NEVRAs for all the projects used to feed the depsolve step
    getProjectNEVRAs :: [T.Text] -> [PackageNEVRA] -> [PackageNEVRA]
    getProjectNEVRAs project_names all_nevras = mapMaybe lookupProject project_names
      where
        lookupProject project_name = find (\e -> pnName e == project_name) all_nevras
