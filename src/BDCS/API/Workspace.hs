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

module BDCS.API.Workspace(workspaceRead,
                          workspaceWrite,
                          workspaceDelete,
                          workspaceDir,
                          WorkspaceError(..))
  where

import           BDCS.API.Recipe(Recipe(..), parseRecipe, recipeTOML, recipeTomlFilename)
import           BDCS.API.Utils(maybeThrow)
import           Control.Conditional(ifM, whenM)
import           Control.Exception
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           GI.Gio(fileGetPath)
import qualified GI.Ggit as Git
import           System.Directory(createDirectoryIfMissing, doesFileExist, removeFile)
import           System.FilePath.Posix((</>))

data WorkspaceError =
    RepoLocationError
  | ParseRecipeError String
  deriving (Eq, Show)

instance Exception WorkspaceError

-- | Create the workspace's path from a Repository and branch
workspaceDir :: Git.Repository -> T.Text -> IO FilePath
workspaceDir repo branch = do
    location <- Git.repositoryGetLocation repo >>= maybeThrow RepoLocationError
    path <- fileGetPath location >>= maybeThrow RepoLocationError
    return $ path </> "workspace" </> T.unpack branch

-- | Read a Recipe from the branch's workspace
-- Returns a Maybe Recipe
-- Can throw WorkspaceError
workspaceRead :: Git.Repository -> T.Text -> T.Text -> IO (Maybe Recipe)
workspaceRead repo branch recipe_name = do
    dir <- workspaceDir repo branch
    createDirectoryIfMissing True dir
    let filename = dir </> T.unpack (recipeTomlFilename $ T.unpack recipe_name)
    ifM (doesFileExist filename)
        (Just <$> readRecipe filename)
        (return Nothing)
  where
    readRecipe :: FilePath -> IO Recipe
    readRecipe filename = do
        toml_in <- TIO.readFile filename
        let erecipe = parseRecipe toml_in
        case erecipe of
            Left e       -> throwIO $ ParseRecipeError e
            Right recipe -> return recipe

workspaceWrite :: Git.Repository -> T.Text -> Recipe -> IO ()
workspaceWrite repo branch recipe = do
    dir <- workspaceDir repo branch
    createDirectoryIfMissing True dir
    let toml_out = T.unpack $ recipeTOML recipe
    let filename = dir </> T.unpack (recipeTomlFilename (rName recipe))
    writeFile filename toml_out

-- | Delete the recipe from the workspace
-- Can throw a WorkspaceError
workspaceDelete :: Git.Repository -> T.Text -> T.Text -> IO ()
workspaceDelete repo branch recipe_name = do
    dir <- workspaceDir repo branch
    let filename = dir </> T.unpack (recipeTomlFilename $ T.unpack recipe_name)
    whenM (doesFileExist filename) (removeFile filename)
