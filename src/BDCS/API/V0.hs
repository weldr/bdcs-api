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
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module BDCS.API.V0(PackageInfo(..),
                   RecipesListResponse(..),
                   RecipesInfoResponse(..),
                   RecipesChangesResponse(..),
                   RecipesDiffResponse(..),
                   RecipesStatusResponse(..),
                   RecipesAPIError(..),
                   RecipeChanges(..),
                   WorkspaceChanges(..),
                   V0API,
                   v0ApiServer)
  where

import           BDCS.API.Error(createApiError)
import           BDCS.API.Recipe
import           BDCS.API.Recipes
import           BDCS.API.TOMLMediaType
import           BDCS.API.Utils(GitLock(..), argify)
import           BDCS.API.Workspace
import           BDCS.DB
import           BDCS.Depclose(depclose)
import           BDCS.Depsolve(formulaToCNF, solveCNF)
import           BDCS.Groups(groupIdToNevra)
import           BDCS.Projects(findProject, getProject)
import qualified Control.Concurrent.ReadWriteLock as RWL
import qualified Control.Exception as CE
import           Control.Monad.Except
import           Data.Aeson
import           Data.List(sortBy)
import           Data.Maybe(fromJust, fromMaybe)
import qualified Data.Text as T
import           Database.Persist.Sql
import           Data.GI.Base(GError(..))
import qualified GI.Ggit as Git
import           Servant
import           Utils.Monad(mapMaybeM)


{-# ANN module ("HLint: ignore Eta reduce"  :: String) #-}

data PackageInfo = PackageInfo
  {  piName    :: T.Text
  ,  piSummary :: T.Text
  } deriving (Eq, Show)

instance ToJSON PackageInfo where
  toJSON PackageInfo{..} = object [
      "name"    .= piName
    , "summary" .= piSummary ]

instance FromJSON PackageInfo where
  parseJSON = withObject "package info" $ \o -> do
    piName    <- o .: "name"
    piSummary <- o .: "summary"
    return PackageInfo{..}


-- | RecipesAPIError is used to report errors with the /recipes/ routes
data RecipesAPIError = RecipesAPIError
  {  raeRecipe  :: T.Text,
     raeMsg     :: T.Text
  } deriving (Eq, Show)

instance ToJSON RecipesAPIError where
  toJSON RecipesAPIError{..} = object [
      "recipe".= raeRecipe
    , "msg"   .= raeMsg ]

instance FromJSON RecipesAPIError where
  parseJSON = withObject "API Error" $ \o -> do
    raeRecipe <- o .: "recipe"
    raeMsg    <- o .: "msg"
    return RecipesAPIError{..}


type V0API = "package"  :> Capture "package" T.Text :> Get '[JSON] PackageInfo
        :<|> "depsolve" :> Capture "package" T.Text :> Get '[JSON] [T.Text]
        :<|> "errtest"  :> Get '[JSON] [T.Text]
        :<|> "recipes"  :> "list" :> Get '[JSON] RecipesListResponse
        :<|> "recipes"  :> "info" :> Capture "recipes" String :> Get '[JSON] RecipesInfoResponse
        :<|> "recipes"  :> "changes" :> Capture "recipes" String
                                     :> QueryParam "offset" Int
                                     :> QueryParam "limit" Int :> Get '[JSON] RecipesChangesResponse
        :<|> "recipes"  :> "new" :> ReqBody '[JSON, TOML] Recipe :> Post '[JSON] RecipesStatusResponse
        :<|> "recipes"  :> "delete" :> Capture "recipe" String :> Delete '[JSON] RecipesStatusResponse
        :<|> "recipes"  :> "undo" :> Capture "recipe" String
                                  :> Capture "commit" String :> Post '[JSON] RecipesStatusResponse
        :<|> "recipes"  :> "workspace" :> ReqBody '[JSON, TOML] Recipe :> Post '[JSON] RecipesStatusResponse
        :<|> "recipes"  :> "tag" :> Capture "recipe" String :> Post '[JSON] RecipesStatusResponse
        :<|> "recipes"  :> "diff" :> Capture "recipe" String
                                  :> Capture "from_commit" String
                                  :> Capture "to_commit" String
                                  :> Get '[JSON] RecipesDiffResponse

v0ApiServer :: GitLock -> ConnectionPool -> Server V0API
v0ApiServer repoLock pool = pkgInfoH
                       :<|> depsolvePkgH
                       :<|> errTestH
                       :<|> recipesListH
                       :<|> recipesInfoH
                       :<|> recipesChangesH
                       :<|> recipesNewH
                       :<|> recipesDeleteH
                       :<|> recipesUndoH
                       :<|> recipesWorkspaceH
                       :<|> recipesTagH
                       :<|> recipesDiffH
  where
    pkgInfoH package     = liftIO $ packageInfo pool package
    depsolvePkgH package = liftIO $ depsolvePkg pool package
    errTestH             = errTest
    recipesListH         = recipesList repoLock "master"
    recipesInfoH recipes = recipesInfo repoLock "master" recipes
    recipesChangesH recipes offset limit = recipesChanges repoLock "master" recipes offset limit
    recipesNewH recipe   = recipesNew repoLock "master" recipe
    recipesDeleteH recipe= recipesDelete repoLock "master" recipe
    recipesUndoH recipe commit = recipesUndo repoLock "master" recipe commit
    recipesWorkspaceH recipe   = recipesWorkspace repoLock "master" recipe
    recipesTagH recipe   = recipesTag repoLock "master" recipe
    recipesDiffH recipe from_commit to_commit = recipesDiff repoLock "master" recipe from_commit to_commit

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

depsolvePkg :: ConnectionPool -> T.Text -> IO [T.Text]
depsolvePkg pool package = do
    result <- runExceptT $ flip runSqlPool pool $ do
        formula <- depclose ["x86_64"] [package]
        solution <- solveCNF (formulaToCNF formula)
        mapMaybeM groupIdToNevra $ map fst $ filter snd solution
    case result of
        Left _            -> return []
        Right assignments -> return assignments

errTest :: Handler [T.Text]
errTest = throwError myError
  where
    myError :: ServantErr
    myError = createApiError err503 "test_api_error" "This is a test of an API Error Response"

data RecipesListResponse = RecipesListResponse {
    rlrRecipes  :: [T.Text],
    rlrOffset   :: Int,
    rlrLimit    :: Int,
    rlrTotal    :: Int
} deriving (Show, Eq)

instance ToJSON RecipesListResponse where
  toJSON RecipesListResponse{..} = object [
      "recipes" .= rlrRecipes
    , "offset"  .= rlrOffset
    , "limit"   .= rlrLimit
    , "total"   .= rlrTotal ]

instance FromJSON RecipesListResponse where
  parseJSON = withObject "/recipes/list response" $ \o -> do
    rlrRecipes <- o .: "recipes"
    rlrOffset  <- o .: "offset"
    rlrLimit   <- o .: "limit"
    rlrTotal   <- o .: "total"
    return RecipesListResponse{..}


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
    -- handleGitErrors :: GitError -> ServantErr
    -- handleGitErrors e = createApiError err500 "recipes_list" ("Git Error: " ++ show e)


data WorkspaceChanges = WorkspaceChanges {
    wcName      :: T.Text,
    wcChanged   :: Bool
} deriving (Show, Eq)
instance ToJSON WorkspaceChanges where
  toJSON WorkspaceChanges{..} = object [
      "name"    .= wcName
    , "changed" .= wcChanged ]

instance FromJSON WorkspaceChanges where
  parseJSON = withObject "workspace changes" $ \o -> do
    wcName    <- o .: "name"
    wcChanged <- o .: "changed"
    return WorkspaceChanges{..}


data RecipesInfoResponse = RecipesInfoResponse {
    rirChanges  :: [WorkspaceChanges],
    rirRecipes  :: [Recipe],
    rirErrors   :: [RecipesAPIError]
} deriving (Show, Eq)

instance ToJSON RecipesInfoResponse where
  toJSON RecipesInfoResponse{..} = object [
      "changes"   .= rirChanges
    , "recipes" .= rirRecipes
    , "errors"  .= rirErrors ]

instance FromJSON RecipesInfoResponse where
  parseJSON = withObject "/recipes/info response" $ \o -> do
    rirChanges <- o .: "changes"
    rirRecipes <- o .: "recipes"
    rirErrors  <- o .: "errors"
    return RecipesInfoResponse{..}


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
    allRecipeInfo :: [T.Text] -> [WorkspaceChanges] -> [Recipe] -> [RecipesAPIError] -> IO ([WorkspaceChanges], [Recipe], [RecipesAPIError])
    allRecipeInfo [] _ _ _ = return ([], [], [])
    allRecipeInfo [recipe_name] changes_list recipes_list errors_list =
                  oneRecipeInfo recipe_name changes_list recipes_list errors_list
    allRecipeInfo (recipe_name:xs) changes_list recipes_list errors_list = do
                  (new_changes, new_recipes, new_errors) <- oneRecipeInfo recipe_name changes_list recipes_list errors_list
                  allRecipeInfo xs new_changes new_recipes new_errors

    oneRecipeInfo :: T.Text -> [WorkspaceChanges] -> [Recipe] -> [RecipesAPIError] -> IO ([WorkspaceChanges], [Recipe], [RecipesAPIError])
    oneRecipeInfo recipe_name changes_list recipes_list errors_list = do
        result <- getRecipeInfo recipe_name
        return (new_changes result, new_recipes result, new_errors result)
      where
        new_errors :: Either String (Bool, Recipe) -> [RecipesAPIError]
        new_errors (Left err)  = RecipesAPIError recipe_name (T.pack err):errors_list
        new_errors (Right _) = errors_list

        new_changes :: Either String (Bool, Recipe) -> [WorkspaceChanges]
        new_changes (Right (changed, _)) = WorkspaceChanges recipe_name changed:changes_list
        new_changes (Left _)             = changes_list

        new_recipes :: Either String (Bool, Recipe) -> [Recipe]
        new_recipes (Right (_, recipe)) = recipe:recipes_list
        new_recipes (Left _)            = recipes_list

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


data RecipeChanges = RecipeChanges {
    rcName      :: T.Text,
    rcChange    :: [CommitDetails],
    rcTotal     :: Int
} deriving (Show, Eq)

instance ToJSON RecipeChanges where
  toJSON RecipeChanges{..} = object [
      "name"   .= rcName
    , "change" .= rcChange
    , "total"  .= rcTotal ]

instance FromJSON RecipeChanges where
  parseJSON = withObject "recipe changes" $ \o -> do
    rcName   <- o .: "name"
    rcChange <- o .: "change"
    rcTotal  <- o .: "total"
    return RecipeChanges{..}


data RecipesChangesResponse = RecipesChangesResponse {
    rcrRecipes  :: [RecipeChanges],
    rcrErrors   :: [RecipesAPIError],
    rcrOffset   :: Int,
    rcrLimit    :: Int
} deriving (Show, Eq)

instance ToJSON RecipesChangesResponse where
  toJSON RecipesChangesResponse{..} = object [
      "recipes" .= rcrRecipes
    , "errors" .= rcrErrors
    , "offset" .= rcrOffset
    , "limit"  .= rcrLimit ]

instance FromJSON RecipesChangesResponse where
  parseJSON = withObject "/recipes/changes/ response" $ \o -> do
    rcrRecipes <- o .: "recipes"
    rcrErrors  <- o .: "errors"
    rcrOffset  <- o .: "offset"
    rcrLimit   <- o .: "limit"
    return RecipesChangesResponse{..}


-- | /api/v0/recipes/changes/<recipes>
-- Return the commit history of the recipes
--
-- The changes for each listed recipe will have offset and limit applied to them.
-- This means that there will be cases where changes will be empty, when offset > total
-- for the recipe.
--
-- If a recipe commit has been tagged as a new revision the `changes` will include a
-- `revision` field set to the revision number. If the commit has not been tagged it
-- will not have this field included.
--
-- > {
-- >     "recipes": [
-- >         {
-- >             "name": "nfs-server",
-- >             "changes": [
-- >                 {
-- >                     "commit": "97d483e8dd0b178efca9a805e5fd8e722c48ac8e",
-- >                     "time": "Wed,  1 Mar 2017 13:29:37 -0800",
-- >                     "summary": "Recipe nfs-server saved"
-- >                 },
-- >                 {
-- >                     "commit": "857e1740f983bf033345c3242204af0ed7b81f37",
-- >                     "time": "Wed,  1 Mar 2017 09:28:53 -0800",
-- >                     "summary": "Recipe nfs-server saved",
-- >                     "revision" : 1
-- >                 }
-- >             ],
-- >             "total": 2
-- >         },
-- >         {
-- >             "name": "ruby",
-- >             "changes": [
-- >                 {
-- >                     "commit": "4b84f072befc3f4debbe1348d6f4b166f7c83d78",
-- >                     "time": "Wed,  1 Mar 2017 13:32:09 -0800",
-- >                     "summary": "Recipe ruby saved"
-- >                 },
-- >                 {
-- >                     "commit": "85999253c1790367a860a344ea622971b7e0a050",
-- >                     "time": "Wed,  1 Mar 2017 13:31:19 -0800",
-- >                     "summary": "Recipe ruby saved"
-- >                 }
-- >             ],
-- >             "total": 2
-- >         }
-- >     ],
-- >     "errors": [
-- >         {
-- >             "recipe": "a-missing-recipe",
-- >             "msg": "Error retrieving a-missing-recipe"
-- >         }
-- >     ]
-- >     "offset": 0,
-- >     "limit": 20
-- > }
--
recipesChanges :: GitLock -> T.Text -> String -> Maybe Int -> Maybe Int -> Handler RecipesChangesResponse
recipesChanges repoLock branch recipe_names moffset mlimit = liftIO $ RWL.withRead (gitRepoLock repoLock) $ do
    let recipe_name_list = map T.pack (argify [recipe_names])
    (changes, errors) <- allRecipeChanges recipe_name_list [] []
    return $ RecipesChangesResponse changes errors offset limit
  where
    allRecipeChanges :: [T.Text] -> [RecipeChanges] -> [RecipesAPIError] -> IO ([RecipeChanges], [RecipesAPIError])
    allRecipeChanges [] _ _ = return ([], [])
    allRecipeChanges [recipe_name] changes_list errors_list =
                     oneRecipeChange recipe_name changes_list errors_list
    allRecipeChanges (recipe_name:xs) changes_list errors_list = do
                     (new_changes, new_errors) <- oneRecipeChange recipe_name changes_list errors_list
                     allRecipeChanges xs new_changes new_errors

    oneRecipeChange :: T.Text -> [RecipeChanges] -> [RecipesAPIError] -> IO ([RecipeChanges], [RecipesAPIError])
    oneRecipeChange recipe_name changes_list errors_list = do
        result <- catch_recipe_changes recipe_name
        return (new_changes result, new_errors result)
      where
        new_changes :: Either String [CommitDetails] -> [RecipeChanges]
        new_changes (Right changes) = RecipeChanges recipe_name (apply_limits changes) (length $ apply_limits changes):changes_list
        new_changes (Left _)        = changes_list

        new_errors :: Either String [CommitDetails] -> [RecipesAPIError]
        new_errors (Left err) = RecipesAPIError recipe_name (T.pack err):errors_list
        new_errors (Right _)  = errors_list

    offset :: Int
    offset = fromMaybe 0 moffset

    limit :: Int
    limit  = fromMaybe 20 mlimit

    apply_limits :: [a] -> [a]
    apply_limits l = take limit $ drop offset l

    catch_recipe_changes :: T.Text -> IO (Either String [CommitDetails])
    catch_recipe_changes recipe_name =
        CE.catches (Right <$> listRecipeCommits (gitRepo repoLock) branch recipe_name)
                   [CE.Handler (\(e :: GitError) -> return $ Left (show e)),
                    CE.Handler (\(e :: GError) -> return $ Left (show e))]


data RecipesStatusResponse = RecipesStatusResponse {
    rsrStatus :: Bool,
    rsrErrors :: [RecipesAPIError]
} deriving (Show, Eq)

instance ToJSON RecipesStatusResponse where
  toJSON RecipesStatusResponse{..} = object [
      "status" .= rsrStatus
    , "errors" .= rsrErrors ]

instance FromJSON RecipesStatusResponse where
  parseJSON = withObject "/recipes/* status response" $ \o -> do
    rsrStatus <- o .: "status"
    rsrErrors <- o .: "errors"
    return RecipesStatusResponse{..}

-- | POST `/api/v0/recipes/new`
-- Create or update a recipe.
--
-- The body of the post is a JSON representation of the recipe, using the same format
-- received by `/api/v0/recipes/info/<recipes>`
recipesNew :: GitLock -> T.Text -> Recipe -> Handler RecipesStatusResponse
recipesNew repoLock branch recipe = liftIO $ RWL.withRead (gitRepoLock repoLock) $ do
    result <- catch_recipe_new
    case result of
        Left err -> return $ RecipesStatusResponse False [RecipesAPIError "Unknown" (T.pack err)]
        Right _  -> return $ RecipesStatusResponse True []
  where
    catch_recipe_new :: IO (Either String Git.OId)
    catch_recipe_new =
        CE.catches (Right <$> commitRecipe (gitRepo repoLock) branch recipe)
                   [CE.Handler (\(e :: GitError) -> return $ Left (show e)),
                    CE.Handler (\(e :: GError) -> return $ Left (show e))]


-- | DELETE /api/v0/recipes/delete/<recipe>
-- Delete the named recipe from the repository
recipesDelete :: GitLock -> T.Text -> String -> Handler RecipesStatusResponse
recipesDelete repoLock branch recipe_name = liftIO $ RWL.withRead (gitRepoLock repoLock) $ do
    result <- catch_recipe_delete
    case result of
        Left err -> return $ RecipesStatusResponse False [RecipesAPIError (T.pack recipe_name) (T.pack err)]
        Right _  -> return $ RecipesStatusResponse True []
  where
    catch_recipe_delete :: IO (Either String Git.OId)
    catch_recipe_delete =
        CE.catches (Right <$> deleteRecipe (gitRepo repoLock) branch (T.pack recipe_name))
                   [CE.Handler (\(e :: GitError) -> return $ Left (show e)),
                    CE.Handler (\(e :: GError) -> return $ Left (show e))]


-- | POST /api/v0/recipes/undo/<recipe>/<commit>
-- Revert a recipe to a previous commit
recipesUndo :: GitLock -> T.Text -> String -> String -> Handler RecipesStatusResponse
recipesUndo repoLock branch recipe_name commit = liftIO $ RWL.withRead (gitRepoLock repoLock) $ do
    result <- catch_recipe_undo
    case result of
        Left err -> return $ RecipesStatusResponse False [RecipesAPIError (T.pack recipe_name) (T.pack err)]
        Right _  -> return $ RecipesStatusResponse True []
  where
    catch_recipe_undo :: IO (Either String Git.OId)
    catch_recipe_undo =
        CE.catches (Right <$> revertRecipe (gitRepo repoLock) branch (T.pack recipe_name) (T.pack commit))
                   [CE.Handler (\(e :: GitError) -> return $ Left (show e)),
                    CE.Handler (\(e :: GError) -> return $ Left (show e))]


-- | POST /api/v0/recipes/workspace
-- Update the temporary recipe workspace
--
-- - The body of the post is a JSON representation of the recipe, using the same format
--   received by `/api/v0/recipes/info/<recipes>` and `/api/v0/recipes/new`
recipesWorkspace :: GitLock -> T.Text -> Recipe -> Handler RecipesStatusResponse
recipesWorkspace repoLock branch recipe = liftIO $ RWL.withRead (gitRepoLock repoLock) $ do
    result <- catch_recipe_ws
    case result of
        Left err -> return $ RecipesStatusResponse False [RecipesAPIError "Unknown" (T.pack err)]
        Right _  -> return $ RecipesStatusResponse True []
  where
    catch_recipe_ws :: IO (Either String ())
    catch_recipe_ws =
        CE.catches (Right <$> workspaceWrite (gitRepo repoLock) branch recipe)
                   [CE.Handler (\(e :: GitError) -> return $ Left (show e)),
                    CE.Handler (\(e :: GError) -> return $ Left (show e))]


-- | POST /api/v0/recipes/tag/<recipe>
-- | Tag the most recent recipe commit as the next revision
recipesTag :: GitLock -> T.Text -> String -> Handler RecipesStatusResponse
recipesTag repoLock branch recipe_name = liftIO $ RWL.withRead (gitRepoLock repoLock) $ do
    result <- catch_recipe_tag
    case result of
        Left  err    -> return $ RecipesStatusResponse False [RecipesAPIError "Unknown" (T.pack err)]
        Right status -> return $ RecipesStatusResponse status []
  where
    catch_recipe_tag :: IO (Either String Bool)
    catch_recipe_tag =
        CE.catches (Right <$> tagRecipeCommit (gitRepo repoLock) branch (T.pack recipe_name))
                   [CE.Handler (\(e :: GitError) -> return $ Left (show e)),
                    CE.Handler (\(e :: GError) -> return $ Left (show e))]


data RecipesDiffResponse = RecipesDiffResponse {
    rdrDiff :: [RecipeDiffEntry]
} deriving (Eq, Show)

instance ToJSON RecipesDiffResponse where
  toJSON RecipesDiffResponse{..} = object [
      "diff" .= rdrDiff ]

instance FromJSON RecipesDiffResponse where
  parseJSON = withObject "/recipes/diff response" $ \o -> do
    rdrDiff <- o .: "diff"
    return RecipesDiffResponse{..}

-- | /api/v0/recipes/diff/<recipe>/<from_commit>/<to_commit>
-- Return the diff between the two recipe commits. Set to_commit to NEWEST to use the newest commit.
--
-- # Arguments
--
-- * `recipe_name` - Recipe name
-- * `from_commit` - The older commit to caclulate the difference from, can also be NEWEST
-- * `to_commit` - The newer commit to calculate the diff. to, can also be NEWEST or WORKSPACE
--
-- # Response
--
-- * JSON response with recipe changes.
--
-- # Errors
--
-- If there is an error retrieving a commit (eg. it cannot find the hash), it will use HEAD
-- instead and log an error.
--
--
-- In addition to the commit hashes listed by a call to `/recipes/changes/<recipe-name>` you
-- can use `NEWEST` to compare the latest commit, and `WORKSPACE` to compare it with
-- the current temporary workspace version of the recipe. eg. to see what the differences
-- are between the current workspace and most recent commit of `http-server` you would call:
--
-- `/recipes/diff/http-server/NEWEST/WORKSPACE`
--
-- Each entry in the response's diff object contains the old recipe value and the new one.
-- If old is null and new is set, then it was added.
-- If new is null and old is set, then it was removed.
-- If both are set, then it was changed.
--
-- The old/new entries will have the name of the recipe field that was changed. This
-- can be one of: Name, Description, Version, Module, or Package.
-- The contents for these will be the old/new values for them.
--
-- In the example below the description and version were changed. The php module's
-- version was changed, the rsync package was removed, and the vim-enhanced package
-- was added.
--
-- # Examples
--
-- > {
-- >     "diff": [
-- >         {
-- >             "old": {
-- >                 "Description": "An example http server with PHP and MySQL support."
-- >             },
-- >             "new": {
-- >                 "Description": "Apache HTTP Server"
-- >             }
-- >         },
-- >         {
-- >             "old": {
-- >                 "Version": "0.0.1"
-- >             },
-- >             "new": {
-- >                 "Version": "0.1.1"
-- >             }
-- >         },
-- >         {
-- >             "old": {
-- >                 "Module": {
-- >                     "name": "php",
-- >                     "version": "5.4.*"
-- >                 }
-- >             },
-- >             "new": {
-- >                 "Module": {
-- >                     "name": "php",
-- >                     "version": "5.6.*"
-- >                 }
-- >             }
-- >         },
-- >         {
-- >             "old": null,
-- >             "new": {
-- >                 "Package": {
-- >                     "name": "vim-enhanced",
-- >                     "version": "8.0.*"
-- >                 }
-- >             }
-- >         },
-- >         {
-- >             "old": {
-- >                 "Package": {
-- >                     "name": "rsync",
-- >                     "version": "3.0.*"
-- >                 }
-- >             },
-- >             "new": null
-- >         }
-- >     ]
-- > }
recipesDiff :: GitLock -> T.Text -> String -> String -> String -> Handler RecipesDiffResponse
recipesDiff repoLock branch recipe_name from_commit to_commit = liftIO $ RWL.withRead (gitRepoLock repoLock) $ do
    -- Setup old_recipe
    -- NEWEST == read the latest git commit for recipe_name
    -- Otherwise try to read the passed-in commit hash string
    old_recipe <- get_recipe from_commit

    -- Setup new_recipe
    -- WORKSPACE == read the recipe's workspace
    -- NEWEST == read the latest git commit for recipe_name
    -- Otherwise try to read the passed-in commit hash string
    new_recipe <- get_recipe to_commit

    case (old_recipe, new_recipe) of
        (Left _, _)     -> return $ RecipesDiffResponse []
        (_, Left _)     -> return $ RecipesDiffResponse []
        (Right o, Right n) -> do
            let diff = recipeDiff o n
            return $ RecipesDiffResponse diff
  where
    get_recipe :: String -> IO (Either String Recipe)
    get_recipe "NEWEST"    = catch_git_recipe (T.pack recipe_name) Nothing
    get_recipe "WORKSPACE" = do
        ws_recipe <- catch_ws_recipe (T.pack recipe_name)
        -- If there is no workspace recipe fall back to most recent commit
        case ws_recipe of
            Just recipe -> return $ Right recipe
            Nothing     -> get_recipe "NEWEST"
    get_recipe commit      = catch_git_recipe (T.pack recipe_name) (Just $ T.pack commit)

    -- | Read the recipe from the workspace, and convert WorkspaceErrors into Nothing
    catch_ws_recipe :: T.Text -> IO (Maybe Recipe)
    catch_ws_recipe name =
        CE.catch (workspaceRead (gitRepo repoLock) branch name)
                 (\(_ :: WorkspaceError) -> return Nothing)

    -- | Read the recipe from git, and convert errors into Left descriptions of what went wrong.
    catch_git_recipe :: T.Text -> Maybe T.Text -> IO (Either String Recipe)
    catch_git_recipe name commit =
        CE.catches (readRecipeCommit (gitRepo repoLock) branch name commit)
                   [CE.Handler (\(e :: GitError) -> return $ Left (show e)),
                    CE.Handler (\(e :: GError) -> return $ Left (show e))]

-- | * `/api/v0/recipes/freeze/<recipes>`
-- |  - Return the contents of the recipe with frozen dependencies instead of expressions.
-- |  - [Example JSON](fn.recipes_freeze.html#examples)
-- | * `/api/v0/recipes/depsolve/<recipes>`
-- |  - Return the recipe and summary information about all of its modules and packages.
-- |  - [Example JSON](fn.recipes_depsolve.html#examples)

