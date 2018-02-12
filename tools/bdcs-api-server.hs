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
{-# LANGUAGE TemplateHaskell #-}

import           BDCS.API.Server(runServer)
import           Cmdline(CliOptions(..),
                         parseArgs)
import           Control.Monad(when)
import           Data.Version (showVersion)
import           Development.GitRev
import           Paths_bdcs_api(version)

main :: IO ()
main = do
    opts <- parseArgs

    when (optShowVersion opts) $ do
        let git_version = $(gitDescribe)
        if git_version == "UNKNOWN" then
            putStrLn ("bdcs-api v" ++ showVersion version)
        else
            putStrLn ("bdcs-api " ++ git_version)

    runServer (optPort opts) (optRecipeRepo opts) (optMetadataDB opts)
