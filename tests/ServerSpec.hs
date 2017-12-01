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

module ServerSpec
  where

import           BDCS.API.Recipe(Recipe(..), RecipeModule(..))
import           BDCS.API.Server
import           BDCS.API.V0
import           Control.Conditional(whenM)
import           Control.Exception(throwIO)
import           Control.Monad.Loops(allM)
import           Data.List(isSuffixOf)
import qualified Data.Text as T
import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client
import           System.Directory(copyFileWithMetadata, createDirectoryIfMissing, doesPathExist,
                                  listDirectory, removeDirectoryRecursive)
import           System.FilePath.Posix((</>))
import           Text.Printf(printf)
import           Test.Hspec

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Use null"  :: String) #-}
{-# ANN module ("HLint: ignore Use head"  :: String) #-}

-- Client API
getStatus :: ClientM ServerStatus
getPackage :: T.Text -> ClientM PackageInfo
getDeps :: T.Text -> ClientM [T.Text]
getErr :: ClientM [T.Text]
getRecipes :: ClientM RecipesListResponse
getRecipesInfo :: String -> ClientM RecipesInfoResponse
getRecipesChanges :: String -> Maybe Int -> Maybe Int -> ClientM RecipesChangesResponse
postRecipesNew :: Recipe -> ClientM RecipesNewResponse
deleteRecipes :: String -> ClientM RecipesDeleteResponse
getStatus :<|> getPackage :<|> getDeps :<|> getErr
          :<|> getRecipes :<|> getRecipesInfo :<|> getRecipesChanges
          :<|> postRecipesNew :<|> deleteRecipes = client proxyAPI


-- Test results, depends on the contents of the ./tests/recipes files.
recipesListResponse :: RecipesListResponse
recipesListResponse = RecipesListResponse ["glusterfs", "http-server", "kubernetes"] 0 0 3

missingRecipeResponse :: RecipesInfoResponse
missingRecipeResponse = RecipesInfoResponse [] [] [RecipesAPIError "missing-recipe" "missing-recipe.toml is not present on branch master"]

httpserverRecipeResponse :: RecipesInfoResponse
httpserverRecipeResponse =
    RecipesInfoResponse [WorkspaceChanges "http-server" False]
                        [Recipe "http-server" (Just "0.2.0") "An example http server with PHP and MySQL support."
                          [RecipeModule "tmux" "2.2",
                           RecipeModule "openssh-server" "6.6.*",
                           RecipeModule "rsync" "3.0.*"]
                          [RecipeModule "httpd" "2.4.*",
                           RecipeModule "mod_auth_kerb" "5.4",
                           RecipeModule "mod_ssl" "2.4.*",
                           RecipeModule "php" "5.4.*",
                           RecipeModule "php-mysql" "5.4.*"]
                        ]
                        []

multipleRecipeResponse :: RecipesInfoResponse
multipleRecipeResponse =
    RecipesInfoResponse [WorkspaceChanges "glusterfs" False,
                         WorkspaceChanges "http-server" False]
                        [Recipe "glusterfs" (Just "0.0.1") "An example GlusterFS server with samba"
                          [RecipeModule "samba" "4.2.*"]
                          [RecipeModule "glusterfs" "3.7.*",
                           RecipeModule "glusterfs-cli" "3.7.*"],
                         Recipe "http-server" (Just "0.2.0") "An example http server with PHP and MySQL support."
                          [RecipeModule "tmux" "2.2",
                           RecipeModule "openssh-server" "6.6.*",
                           RecipeModule "rsync" "3.0.*"]
                          [RecipeModule "httpd" "2.4.*",
                           RecipeModule "mod_auth_kerb" "5.4",
                           RecipeModule "mod_ssl" "2.4.*",
                           RecipeModule "php" "5.4.*",
                           RecipeModule "php-mysql" "5.4.*"]
                        ]
                        []

errorRecipeResponse :: RecipesInfoResponse
errorRecipeResponse =
    RecipesInfoResponse [WorkspaceChanges "glusterfs" False,
                         WorkspaceChanges "http-server" False]
                        [Recipe "glusterfs" (Just "0.0.1") "An example GlusterFS server with samba"
                          [RecipeModule "samba" "4.2.*"]
                          [RecipeModule "glusterfs" "3.7.*",
                           RecipeModule "glusterfs-cli" "3.7.*"],
                         Recipe "http-server" (Just "0.2.0") "An example http server with PHP and MySQL support."
                          [RecipeModule "tmux" "2.2",
                           RecipeModule "openssh-server" "6.6.*",
                           RecipeModule "rsync" "3.0.*"]
                          [RecipeModule "httpd" "2.4.*",
                           RecipeModule "mod_auth_kerb" "5.4",
                           RecipeModule "mod_ssl" "2.4.*",
                           RecipeModule "php" "5.4.*",
                           RecipeModule "php-mysql" "5.4.*"]
                        ]
                        [RecipesAPIError "missing-recipe" "missing-recipe.toml is not present on branch master"]

recipesNewResponse :: RecipesNewResponse
recipesNewResponse = RecipesNewResponse True []

aTestRecipe :: Recipe
aTestRecipe = Recipe "A Test Recipe" (Just "0.0.1") "A simple recipe to use for testing"
                     [RecipeModule "rsync" "3.0.*"]
                     [RecipeModule "httpd" "2.4.*"]

-- Post 10 changes to the test recipe
postMultipleChanges :: ClientM Bool
postMultipleChanges = allM newVersion [0..10]
  where
    newVersion :: Integer -> ClientM Bool
    newVersion patch = status_ok <$> postRecipesNew aTestRecipe {rVersion = Just $ patchedVersion patch}

    patchedVersion :: Integer -> String
    patchedVersion = printf "0.1.%d"

    status_ok :: RecipesNewResponse -> Bool
    status_ok = rnrStatus

-- If it has 0 errors, 1 change named http-server, 0 offset and a limit of 20 it passes
recipesChangesTest1 :: ClientM Bool
recipesChangesTest1 = do
    response <- getRecipesChanges "http-server" Nothing Nothing
    return $ length_ok response && name_ok response && error_ok response && limits_ok response
  where
    length_ok :: RecipesChangesResponse -> Bool
    length_ok response = length (rcrRecipes response) == 1

    name_ok :: RecipesChangesResponse -> Bool
    name_ok response = rcName (rcrRecipes response !! 0) == "http-server"

    error_ok :: RecipesChangesResponse -> Bool
    error_ok response = length (rcrErrors response) == 0

    limits_ok :: RecipesChangesResponse -> Bool
    limits_ok response = rcrOffset response == 0 && rcrLimit response == 20

-- If it has 0 errors, 1 change named http-server, 1 named glusterfs, 0 offset and a limit of 20 it passes
recipesChangesTest2 :: ClientM Bool
recipesChangesTest2 = do
    response <- getRecipesChanges "http-server,glusterfs" Nothing Nothing
    return $ length_ok response && name_ok response && error_ok response && limits_ok response
  where
    length_ok :: RecipesChangesResponse -> Bool
    length_ok response = length (rcrRecipes response) == 2

    name_ok :: RecipesChangesResponse -> Bool
    name_ok response = rcName (rcrRecipes response !! 0) == "glusterfs" &&
                       rcName (rcrRecipes response !! 1) == "http-server"

    error_ok :: RecipesChangesResponse -> Bool
    error_ok response = length (rcrErrors response) == 0

    limits_ok :: RecipesChangesResponse -> Bool
    limits_ok response = rcrOffset response == 0 && rcrLimit response == 20

-- | Check that limit and offset are parsed correctly
--
-- This must be called after posting > 10 changes to the recipe
recipesChangesTest3 :: ClientM Bool
recipesChangesTest3 = do
    response <- getRecipesChanges "A Test Recipe" (Just 5) (Just 5)
    return $ limits_ok response && length_ok response && name_ok response && total_ok response
  where
    limits_ok :: RecipesChangesResponse -> Bool
    limits_ok response = rcrOffset response == 5 && rcrLimit response == 5

    -- Should only be 1 recipe in the response
    length_ok :: RecipesChangesResponse -> Bool
    length_ok response = length (rcrRecipes response) == 1

    name_ok :: RecipesChangesResponse -> Bool
    name_ok response = rcName (rcrRecipes response !! 0) == "A Test Recipe"

    total_ok :: RecipesChangesResponse -> Bool
    total_ok response = rcTotal (rcrRecipes response !! 0) == 5


-- | Setup the temporary repo directory with some example recipes
--
-- If the directory exists it is first removed.
-- Then example recipes are copied into it
setupTempRepoDir :: FilePath -> FilePath -> IO ()
setupTempRepoDir exampleRecipes gitRepoPath = do
        whenM (doesPathExist gitRepoPath) (removeDirectoryRecursive gitRepoPath)
        createDirectoryIfMissing True gitRepoPath
        copyTOMLFiles exampleRecipes gitRepoPath
  where
    -- Copy files ending with .toml from one directory to another
    copyTOMLFiles :: FilePath -> FilePath -> IO ()
    copyTOMLFiles fromDir toDir = do
        files <- filter (".toml" `isSuffixOf`) <$> listDirectory fromDir
        let fromFiles = map (fromDir </>) files
        let toFiles = map (toDir </>) files
        mapM_ (\(from, to) -> copyFileWithMetadata from to) $ zip fromFiles toFiles


spec :: Spec
spec = do
    describe "Setup" $
        it "Setup the temporary test directory" $
            setupTempRepoDir "./tests/recipes/" "/var/tmp/bdcs-tmp-recipes/"

    describe "/api" $
        -- NOTE that mkApp is executed for EACH of the 'it' sections
        withClient (mkApp "/var/tmp/bdcs-tmp-recipes/" "/var/tmp/test-bdcs.db") $ do
            it "API Status" $ \env ->
                try env getStatus `shouldReturn` ServerStatus "0.0.0" "0" "0" False

            it "list the available recipes" $ \env ->
                try env getRecipes `shouldReturn` recipesListResponse

            it "Get a non-existant recipe's info" $ \env ->
                try env (getRecipesInfo "missing-recipe") `shouldReturn` missingRecipeResponse

            it "Get the http-server recipe's info" $ \env ->
                try env (getRecipesInfo "http-server") `shouldReturn` httpserverRecipeResponse

            it "Get multiple recipe's info" $ \env ->
                try env (getRecipesInfo "http-server,glusterfs") `shouldReturn` multipleRecipeResponse

            it "Get http-server recipe and missing-recipe info" $ \env ->
                try env (getRecipesInfo "http-server,missing-recipe,glusterfs") `shouldReturn` errorRecipeResponse
            it "Post a test recipe" $ \env ->
                try env (postRecipesNew aTestRecipe) `shouldReturn` recipesNewResponse

            it "Post several changes to test recipe" $ \env ->
                try env postMultipleChanges `shouldReturn` True

            it "Get changes to http-server recipe" $ \env ->
                try env recipesChangesTest1 `shouldReturn` True

            it "Get changes to http-server and glusterfs recipes" $ \env ->
                try env recipesChangesTest2 `shouldReturn` True

--            it "blah blah" $ \env ->
--                try env (getRecipesChanges "A Test Recipe" (Just 5) (Just 5)) `shouldReturn` RecipesChangesResponse [] [] 0 0

            it "Check offset and limit usage" $ \env ->
                try env recipesChangesTest3 `shouldReturn` True

    describe "cleanup" $
        it "Remove the temporary directory" $
            removeDirectoryRecursive "/var/tmp/bdcs-tmp-recipes/"


withClient :: IO Application -> SpecWith ClientEnv -> SpecWith ()
withClient x innerSpec =
    beforeAll (newManager defaultManagerSettings) $
        flip aroundWith innerSpec $ \action mgr ->
            testWithApplication x $ \port -> do
                let base = BaseUrl Http "localhost" port ""
                action (ClientEnv mgr base)

type Host = (Manager, BaseUrl)

try :: ClientEnv -> ClientM a -> IO a
try clientEnv action = either throwIO return =<< runClientM action clientEnv
