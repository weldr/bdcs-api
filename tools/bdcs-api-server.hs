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

import           BDCS.API.Server(runServer)
import           BDCS.API.Version(buildVersion)
import           Cmdline(CliOptions(..),
                         parseArgs)
import           Control.Monad(when)

main :: IO ()
main = do
    opts <- parseArgs

    when (optShowVersion opts) $ putStrLn ("bdcs-api " ++ buildVersion)

    runServer (optPort opts) (optBDCS opts) (optRecipeRepo opts) (optMetadataDB opts)
