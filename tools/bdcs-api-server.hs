{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

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

import           BDCS.API.Server(SocketException(..), runServer)
import           BDCS.API.Version(buildVersion)
import           Cmdline(CliOptions(..), parseArgs)
import qualified Control.Exception as CE
import           Control.Monad(when)

main :: IO ()
main = do
    CliOptions{..} <- parseArgs

    when optShowVersion $ putStrLn ("bdcs-api " ++ buildVersion)

    CE.catch (runServer optSocketPath optSocketGroup optBDCS optRecipeRepo optMetadataDB)
             (\case
                  BadFileDescriptor -> putStrLn "Bad value provided in $LISTEN_FDS"
                  BadGroup g        -> putStrLn $ "Provided group does not exist: " ++ g
                  NoSocketError     -> putStrLn "One of $LISTEN_FDS or -s <socket> must be provided")
