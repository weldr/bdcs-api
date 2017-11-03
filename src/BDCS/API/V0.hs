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
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module BDCS.API.V0(DbTest(..),
                   PackageInfo(..),
                   RecipesListResponse(..),
                   RecipesInfoResponse(..),
                   RecipesAPIError(..),
                   WorkspaceChanges(..),
                   V0API,
                   v0ApiServer)
  where

import           BDCS.API.Error(createApiError)
import           BDCS.API.Recipe
import           BDCS.API.Recipes
import           BDCS.API.Utils(GitLock(..), argify)
import           BDCS.API.Workspace
import           BDCS.DB
import           BDCS.Depclose(depclose)
import           BDCS.Depsolve(formulaToCNF, solveCNF)
import           BDCS.Groups(groupIdToNevra)
import           Utils.Monad(mapMaybeM)
import           BDCS.Projects(findProject, getProject)
import qualified Control.Concurrent.ReadWriteLock as RWL
import qualified Control.Exception as CE
import           Control.Monad.Except
import           Control.Monad.Logger(runStderrLoggingT)
import           Control.Monad.Reader
import           Data.Aeson
import           Data.List(intercalate, sortBy)
import           Data.Maybe(fromJust, listToMaybe)
import           Data.String.Conversions(cs)
import qualified Data.Text as T
import           Data.Time.Calendar
import           Database.Persist
import           Database.Persist.Sql
import           Database.Persist.Sqlite
import           GHC.Generics
import           Data.GI.Base(GError(..), gerrorMessage)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Network.Wai.Middleware.Servant.Options
import           Servant


{-# ANN module ("HLint: ignore Eta reduce"  :: String) #-}

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

-- | RecipesAPIError is used to report errors with the /recipes/ routes
data RecipesAPIError = RecipesAPIError
  {  recipe     :: T.Text,
     msg        :: T.Text
  } deriving (Generic, Eq, Show)

instance ToJSON RecipesAPIError
instance FromJSON RecipesAPIError

type V0API = "package"  :> Capture "package" T.Text :> Get '[JSON] PackageInfo
        :<|> "dbtest"   :> Get '[JSON] DbTest
        :<|> "depsolve" :> Capture "package" T.Text :> Get '[JSON] [T.Text]
        :<|> "errtest"  :> Get '[JSON] [T.Text]
        :<|> "recipes"  :> "list" :> Get '[JSON] RecipesListResponse
        :<|> "recipes"  :> "info" :> Capture "recipes" String :> Get '[JSON] RecipesInfoResponse

v0ApiServer :: GitLock -> ConnectionPool -> Server V0API
v0ApiServer repoLock pool = pkgInfoH
                       :<|> dbTestH
                       :<|> depsolvePkgH
                       :<|> errTestH
                       :<|> recipesListH
                       :<|> recipesInfoH
  where
    pkgInfoH package     = liftIO $ packageInfo pool package
    dbTestH              = liftIO $ dbTest pool
    depsolvePkgH package = liftIO $ depsolvePkg pool package
    errTestH             = errTest
    recipesListH         = recipesList repoLock "master"
    recipesInfoH recipes = recipesInfo repoLock "master" recipes

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

depsolvePkg :: ConnectionPool -> T.Text -> IO [T.Text]
depsolvePkg pool package = do
    result <- runExceptT $ flip runSqlPool pool $ do
        formula <- depclose ["x86_64"] [package]
        solution <- solveCNF (formulaToCNF formula)
        mapMaybeM groupIdToNevra $ map fst $ filter snd solution
    case result of
        Left e            -> return []
        Right assignments -> return assignments

errTest :: Handler [T.Text]
errTest = throwError myError
  where
    myError :: ServantErr
    myError = createApiError err503 "test_api_error" "This is a test of an API Error Response"

data RecipesListResponse = RecipesListResponse {
    recipes     :: [T.Text],
    offset      :: Int,
    limit       :: Int,
    total       :: Int
} deriving (Generic, Show, Eq)
instance ToJSON RecipesListResponse
instance FromJSON RecipesListResponse

-- | /api/v0/recipes/list
-- List the names of the available recipes
--
-- >  {
-- >      "recipes": [
-- >          "development",
-- >          "glusterfs",
-- >          "http-server",
-- >          "jboss",
-- >          "kubernetes",
-- >          "octave",
-- >      ],
-- >      "offset": 0,
-- >      "limit": 20,
-- >      "total": 6
-- >  }
recipesList :: GitLock -> T.Text -> Handler RecipesListResponse
recipesList repoLock branch = liftIO $ RWL.withRead (gitRepoLock repoLock) $ do
    -- TODO Figure out how to catch GitError and throw a ServantErr
    filenames <- listBranchFiles (gitRepo repoLock) branch
    let recipes = sortBy caseInsensitive $ map (T.dropEnd 5) filenames
    return $ RecipesListResponse recipes 0 0 (length recipes)
  where
    caseInsensitive a b = T.toCaseFold a `compare` T.toCaseFold b
    handleGitErrors :: GitError -> ServantErr
    handleGitErrors e = createApiError err500 "recipes_list" ("Git Error: " ++ show e)


data WorkspaceChanges = WorkspaceChanges {
    name        :: T.Text,
    changed     :: Bool
} deriving (Generic, Show, Eq)
instance ToJSON WorkspaceChanges
instance FromJSON WorkspaceChanges

data RecipesInfoResponse = RecipesInfoResponse {
    changes     :: [WorkspaceChanges],
    recipes     :: [Recipe],
    errors      :: [RecipesAPIError]
} deriving (Generic, Show, Eq)
instance ToJSON RecipesInfoResponse
instance FromJSON RecipesInfoResponse

-- | /api/v0/recipes/info/<recipes>
-- Return the contents of the recipe, or a list of recipes
--
-- The 'errors' list may be empty, or may include recipe-specific errors if
-- there was a problem retrieving it.
--
-- > {
-- >     "changes": [
-- >         {
-- >             "name": "recipe-test",
-- >             "changed": true
-- >         },
-- >     ],
-- >     "recipes": [
-- >         {
-- >             "name": "http-server",
-- >             "description": "An example http server with PHP and MySQL support.",
-- >             "version": "0.0.1",
-- >             "modules": [
-- >                 {
-- >                     "name": "httpd",
-- >                     "version": "2.4.*"
-- >                 },
-- >                 {
-- >                     "name": "mod_auth_kerb",
-- >                     "version": "5.4"
-- >                 },
-- >                 {
-- >                     "name": "mod_ssl",
-- >                     "version": "2.4.*"
-- >                 },
-- >                 {
-- >                     "name": "php",
-- >                     "version": "5.4.*"
-- >                 },
-- >                 {
-- >                     "name": "php-mysql",
-- >                     "version": "5.4.*"
-- >                 }
-- >             ],
-- >             "packages": [
-- >                 {
-- >                     "name": "tmux",
-- >                     "version": "2.2"
-- >                 },
-- >                 {
-- >                     "name": "openssh-server",
-- >                     "version": "6.6.*"
-- >                 },
-- >                 {
-- >                     "name": "rsync",
-- >                     "version": "3.0.*"
-- >                 }
-- >             ]
-- >         },
-- >     "errors": [
-- >         {
-- >             "recipe": "a-missing-recipe",
-- >             "msg": "Error retrieving a-missing-recipe"
-- >         }
-- >     ]
-- > }
--
recipesInfo :: GitLock -> T.Text -> String -> Handler RecipesInfoResponse
recipesInfo repoLock branch recipe_names = liftIO $ RWL.withRead (gitRepoLock repoLock) $ do
    let recipe_name_list = map T.pack (argify [recipe_names])
    (changes, recipes, errors) <- allRecipeInfo recipe_name_list [] [] []
    return $ RecipesInfoResponse changes recipes errors
  where
    allRecipeInfo [] _ _ _ = return ([], [], [])
    allRecipeInfo [recipe_name] changes_list recipes_list errors_list =
                  oneRecipeInfo recipe_name changes_list recipes_list errors_list
    allRecipeInfo (recipe_name:xs) changes_list recipes_list errors_list = do
                  (new_changes, new_recipes, new_errors) <- oneRecipeInfo recipe_name changes_list recipes_list errors_list
                  allRecipeInfo xs new_changes new_recipes new_errors

    oneRecipeInfo recipe_name changes_list recipes_list errors_list = do
        result <- getRecipeInfo recipe_name
        return (new_changes result, new_recipes result, new_errors result)
      where
        new_errors result = case result of
            Left  err    -> RecipesAPIError recipe_name (T.pack err):errors_list
            Right (_, _) -> errors_list

        new_changes result = case result of
            Left  _            -> changes_list
            Right (changed, _) -> WorkspaceChanges recipe_name changed:changes_list

        new_recipes result = case result of
            Left  _           -> recipes_list
            Right (_, recipe) -> recipe:recipes_list

    -- Get the recipe from the workspace or from git
    getRecipeInfo :: T.Text -> IO (Either String (Bool, Recipe))
    getRecipeInfo recipe_name = do
        --   read the workspace recipe if it exists, errors are mapped to Nothing
        ws_recipe <- catch_ws_recipe recipe_name
        --   read the git recipe (if it exists), Errors are mapped to Left
        git_recipe <- catch_git_recipe recipe_name

        case (ws_recipe, git_recipe) of
            (Nothing,     Left e)       -> return $ Left e
            (Just recipe, Left _)       -> return $ Right (True, recipe)
            (Nothing,     Right recipe) -> return $ Right (False, recipe)
            (Just ws_r,   Right git_r)  -> return $ Right (ws_r == git_r, ws_r)

    -- | Read the recipe from the workspace, and convert WorkspaceErrors into Nothing
    catch_ws_recipe :: T.Text -> IO (Maybe Recipe)
    catch_ws_recipe recipe_name =
        CE.catch (workspaceRead (gitRepo repoLock) branch recipe_name)
                 (\(_ :: WorkspaceError) -> return Nothing)

    -- | Read the recipe from git, and convert errors into Left descriptions of what went wrong.
    catch_git_recipe :: T.Text -> IO (Either String Recipe)
    catch_git_recipe recipe_name =
        CE.catches (readRecipeCommit (gitRepo repoLock) branch recipe_name Nothing)
                   [CE.Handler (\(e :: GitError) -> return $ Left (show e)),
                    CE.Handler (\(e :: GError) -> return $ Left (show e))]


-- | * `/api/v0/recipes/freeze/<recipes>`
-- |  - Return the contents of the recipe with frozen dependencies instead of expressions.
-- |  - [Example JSON](fn.recipes_freeze.html#examples)



-- | * `/api/v0/recipes/changes/<recipes>`
-- |  - Return the commit history of the recipes
-- |  - [Example JSON](fn.recipes_changes.html#examples)
-- |  - [Optional filter parameters](../index.html#optional-filter-parameters)
-- | * `/api/v0/recipes/diff/<recipe>/<from_commit>/<to_commit>`
-- |  - Return the diff between the two recipe commits. Set to_commit to NEWEST to use the newest commit.
-- |  - [Example JSON](fn.recipes_diff.html#examples)
-- | * `/api/v0/recipes/depsolve/<recipes>`
-- |  - Return the recipe and summary information about all of its modules and packages.
-- |  - [Example JSON](fn.recipes_depsolve.html#examples)
-- | * POST `/api/v0/recipes/new`
-- |  - Create or update a recipe.
-- |  - The body of the post is a JSON representation of the recipe, using the same format
-- |    received by `/api/v0/recipes/info/<recipes>`
-- |  - [Example JSON](fn.recipes_new.html#examples)
-- | * DELETE `/api/v0/recipes/delete/<recipe>`
-- |  - Delete the named recipe from the repository
-- |  - [Example JSON](fn.recipes_delete.html#examples)
-- | * POST `/api/v0/recipes/undo/<recipe>/<commit>`
-- |  - Revert a recipe to a previous commit
-- |  - [Example JSON](fn.recipes_undo.html#examples)
-- | * POST `/api/v0/recipes/workspace`
-- |  - Update the temporary recipe workspace
-- |  - The body of the post is a JSON representation of the recipe, using the same format
-- |    received by `/api/v0/recipes/info/<recipes>` and `/api/v0/recipes/new`
-- |  - [Example JSON](fn.recipes_workspace.html#examples)
-- | * POST `/api/v0/recipes/tag/<recipe>`
-- |  - Tag the most recent recipe commit as the next revision
-- |  - [Example](fn.recipes_tag.html)

