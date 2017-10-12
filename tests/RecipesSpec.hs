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

import           BDCS.API.Recipes(runGitRepoTests, runWorkspaceTests)
import           Test.Hspec


{-# ANN module ("HLint: ignore Redundant do Found" :: String) #-}
spec :: Spec
spec =
    describe "Recipes" $ do
        it "Ran a series of tests on storing Recipes in a Git repo" $
            runGitRepoTests `shouldReturn` True

        it "Ran a series of tests on storing Recipes in a Workspace" $
            runWorkspaceTests `shouldReturn` True
