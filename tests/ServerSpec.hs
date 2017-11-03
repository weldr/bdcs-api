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
import           Control.Exception (throwIO)
import qualified Data.Text as T
import           Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import           Network.HTTP.Types
import           Network.Wai (Application)
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.Client
import           Test.Hspec

-- Client API
getStatus :: ClientM ServerStatus
getPackage :: T.Text -> ClientM PackageInfo
getDbTest :: ClientM DbTest
getDeps :: T.Text -> ClientM [T.Text]
getErr :: ClientM [T.Text]
getRecipes :: ClientM RecipesListResponse
getRecipesInfo :: String -> ClientM RecipesInfoResponse
getStatus :<|> getPackage :<|> getDbTest :<|> getDeps :<|> getErr
          :<|> getRecipes :<|> getRecipesInfo = client proxyAPI


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


spec :: Spec
spec =
    describe "/api" $
        withClient (mkApp "/var/tmp/recipes" "/var/tmp/test-bdcs.db") $ do
            it "API Status" $ \env ->
                try env getStatus `shouldReturn` ServerStatus "0.0.0" "0" "0" False

            it "list the available recipes" $ \env ->
                try env getRecipes `shouldReturn` recipesListResponse

            it "Get a non-existant recipe's info" $ \env ->
                try env (getRecipesInfo "missing-recipe")  `shouldReturn` missingRecipeResponse

            it "Get the http-server recipe's info" $ \env ->
                try env (getRecipesInfo "http-server")  `shouldReturn` httpserverRecipeResponse

            it "Get multiple recipe's info" $ \env ->
                try env (getRecipesInfo "http-server,glusterfs")  `shouldReturn` multipleRecipeResponse

            it "Get http-server recipe and missing-recipe info" $ \env ->
                try env (getRecipesInfo "http-server,missing-recipe,glusterfs")  `shouldReturn` errorRecipeResponse

withClient :: IO Application -> SpecWith ClientEnv -> SpecWith ()
withClient x innerSpec =
    beforeAll (newManager defaultManagerSettings) $
        flip aroundWith innerSpec $ \action manager ->
            testWithApplication x $ \port -> do
                let baseUrl = BaseUrl Http "localhost" port ""
                action (ClientEnv manager baseUrl)

type Host = (Manager, BaseUrl)

try :: ClientEnv -> ClientM a -> IO a
try clientEnv action = either throwIO return =<< runClientM action clientEnv
