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

module RecipesSpec
  where

import           BDCS.API.Recipes(runGitRepoTests, runWorkspaceTests, recipeDiff,
                                  RecipeDiffType(..), RecipeDiffEntry(..))
import           BDCS.API.Recipe(Recipe(..), RecipeModule(..))
import           Test.Hspec


-- | Recipe to be used for diff test
testOldRecipe :: Recipe
testOldRecipe =
    Recipe {rName = "test-server",
            rVersion = Just "0.1.2",
            rDescription = "Testing diff of Recipe records",
            rPackages = [RecipeModule {rmName = "tmux", rmVersion = "2.2"},
                         RecipeModule {rmName = "openssh-server", rmVersion = "6.6.*"},
                         RecipeModule {rmName = "rsync", rmVersion = "3.0.*"}],
            rModules = [RecipeModule {rmName = "httpd", rmVersion = "2.4.*"},
                        RecipeModule {rmName = "mod_auth_kerb", rmVersion = "5.4"},
                        RecipeModule {rmName = "mod_ssl", rmVersion = "2.4.*"},
                        RecipeModule {rmName = "php", rmVersion = "5.4.*"},
                        RecipeModule {rmName = "php-mysql", rmVersion = "5.4.*"}]
    }

-- | Recipe to be used for diff test
testNewRecipe :: Recipe
testNewRecipe =
    Recipe {rName = "test-server",
            rVersion = Just "0.1.3",
            rDescription = "Testing diff of Recipe records, new version",
            rPackages = [RecipeModule {rmName = "tmux", rmVersion = "2.2"},
                         RecipeModule {rmName = "openssh-server", rmVersion = "6.7.*"},
                         RecipeModule {rmName = "vim-enhanced", rmVersion = "8.0.*"}],
            rModules = [RecipeModule {rmName = "httpd", rmVersion = "2.4.*"},
                        RecipeModule {rmName = "mod_auth_kerb", rmVersion = "5.5"},
                        RecipeModule {rmName = "mod_ssl", rmVersion = "2.4.*"},
                        RecipeModule {rmName = "php", rmVersion = "5.4.*"},
                        RecipeModule {rmName = "perl", rmVersion = "6.0"}]
    }

testDiffResults :: [RecipeDiffEntry]
testDiffResults = [RecipeDiffEntry {rdeOld = Description {rdtDescription = "Testing diff of Recipe records"},
                                    rdeNew = Description {rdtDescription = "Testing diff of Recipe records, new version"}},
                   RecipeDiffEntry {rdeOld = Version {rdtVersion = Just "0.1.2"},
                                    rdeNew = Version {rdtVersion = Just "0.1.3"}},
                   RecipeDiffEntry {rdeOld = Module {rdtModule = RecipeModule {rmName = "php-mysql", rmVersion = "5.4.*"}},
                                    rdeNew = None},
                   RecipeDiffEntry {rdeOld = None,
                                    rdeNew = Module {rdtModule = RecipeModule {rmName = "perl", rmVersion = "6.0"}}},
                   RecipeDiffEntry {rdeOld = Module {rdtModule = RecipeModule {rmName = "mod_auth_kerb", rmVersion = "5.4"}}, 
                                    rdeNew = Module {rdtModule = RecipeModule {rmName = "mod_auth_kerb", rmVersion = "5.5"}}},
                   RecipeDiffEntry {rdeOld = Package {rdtPackage = RecipeModule {rmName = "rsync", rmVersion = "3.0.*"}}, 
                                    rdeNew = None},
                   RecipeDiffEntry {rdeOld = None,
                                    rdeNew = Package {rdtPackage = RecipeModule {rmName = "vim-enhanced", rmVersion = "8.0.*"}}},
                   RecipeDiffEntry {rdeOld = Package {rdtPackage = RecipeModule {rmName = "openssh-server", rmVersion = "6.6.*"}},
                                    rdeNew = Package {rdtPackage = RecipeModule {rmName = "openssh-server", rmVersion = "6.7.*"}}}]


{-# ANN module ("HLint: ignore Redundant do Found" :: String) #-}
spec :: Spec
spec =
    describe "Recipes" $ do
        it "Ran a series of tests on storing Recipes in a Git repo" $
            runGitRepoTests `shouldReturn` True

        it "Ran a series of tests on storing Recipes in a Workspace" $
            runWorkspaceTests `shouldReturn` True

        it "Test Recipe Diff" $
            (recipeDiff testOldRecipe testNewRecipe) `shouldBe` testDiffResults
