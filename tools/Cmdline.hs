-- Copyright (C) 2017 Red Hat, Inc.
--
-- This file is part of bdcs-cli.
--
-- bdcs-cli is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- bdcs-cli is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with bdcs-cli.  If not, see <http://www.gnu.org/licenses/>.
module Cmdline(CliOptions(..),
               parseArgs,
               usage)
  where

import           System.Console.GetOpt
import           System.Environment(getArgs)

--
-- Commandline parsing
--

data CliOptions = CliOptions
    { optVerbose     :: Bool
    , optShowVersion :: Bool
    , optPort        :: Int
    , optHostIP      :: String
    } deriving Show

defaultOptions :: CliOptions
defaultOptions    = CliOptions
    { optVerbose     = False
    , optShowVersion = False
    , optPort        = 8000
    , optHostIP      = "0.0.0.0"
    }

cliOptions :: [OptDescr (CliOptions -> CliOptions)]
cliOptions =
    [ Option ['v']     ["verbose"]
        (NoArg (\opts -> opts { optVerbose = True }))
        "Verbose output"
    , Option ['V','?'] ["version"]
        (NoArg (\opts -> opts { optShowVersion = True }))
        "show version number"
    , Option ['p']     ["port"]
        (ReqArg (\port opts -> opts { optPort = read port }) "PORT")
        "API Server port"
    , Option ['h']     ["host"]
        (ReqArg (\host opts -> opts { optHostIP = host }) "HOST")
        "API Host IP"
    ]

cliHeader :: String
cliHeader = "Usage: bdcs-api-server [OPTIONS...] <METADATA-DB> <RECIPE-REPO>"

parseOpts :: [String] -> IO (CliOptions, [String])
parseOpts argv =
    case getOpt Permute cliOptions argv of
        (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo cliHeader cliOptions))


helpText :: String
helpText = "\
\\n"

usage :: IO ()
usage = do
    putStrLn $ usageInfo cliHeader cliOptions
    putStr helpText

parseArgs :: IO (CliOptions, [String])
parseArgs = do
    args <- getArgs
    parseOpts args
