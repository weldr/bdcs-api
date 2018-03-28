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
import           Text.Printf(printf)

--
-- Commandline parsing
--

data CliOptions = CliOptions
    { optVerbose     :: Bool
    , optShowVersion :: Bool
    , optPort        :: Int
    , optHostIP      :: String
    , optLogfile     :: FilePath
    , optMockfiles   :: FilePath
    , optBDCS        :: FilePath
    , optMetadataDB  :: FilePath
    , optRecipeRepo  :: FilePath
    } deriving Show

defaultOptions :: CliOptions
defaultOptions    = CliOptions
    { optVerbose     = False
    , optShowVersion = False
    , optPort        = 4000
    , optHostIP      = "127.0.0.1"
    , optLogfile     = "/var/log/bdcs-api.log"
    , optMockfiles   = "/var/tmp/bdcs-mockfiles"
    , optBDCS        = "/mddb/cs.repo/"
    , optMetadataDB  = ""
    , optRecipeRepo  = ""
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
        (printf "Port to bind to (%d)" $ optPort defaultOptions)
    , Option ['h']     ["host"]
        (ReqArg (\host opts -> opts { optHostIP = host }) "HOSTNAME|IP")
        (printf "Host or IP to bind to (%s)" $ optHostIP defaultOptions)
    , Option ['l']     ["log"]
        (ReqArg (\logfile opts -> opts { optLogfile = logfile }) "LOGFILE")
        (printf "Path to JSON logfile (%s)" $ optLogfile defaultOptions)
    , Option ['m']     ["mockfiles"]
        (ReqArg (\mockfiles opts -> opts { optMockfiles = mockfiles }) "MOCKFILES")
        (printf "Path to JSON files used for /api/mock/ paths (%s)" $ optMockfiles defaultOptions)
    , Option ['b']     ["bdcs"]
        (ReqArg (\bdcs opts -> opts { optBDCS = bdcs }) "BDCS")
        (printf "Path to the content store directory (%s)" $ optBDCS defaultOptions)
    ]

cliHeader :: String
cliHeader = "Usage: bdcs-api-server [OPTIONS...] <METADATA-DB> <BLUEPRINT-REPO>"

parseOpts :: [String] -> IO CliOptions
parseOpts argv =
    case getOpt Permute cliOptions argv of
        (o, [mddb, recipes], []) -> return $ resolveDefaults (o ++ [mddbOption mddb, recipesOption recipes])
        (_, _, [])               -> ioError $ userError $ usageInfo cliHeader cliOptions
        (_,_,errs)               -> ioError $ userError $ concat errs ++ usageInfo cliHeader cliOptions
 where
    resolveDefaults o = foldl (flip id) defaultOptions o
    mddbOption    m opts = opts { optMetadataDB = m }
    recipesOption r opts = opts { optRecipeRepo = r }

helpText :: String
helpText = "\
\\n"

usage :: IO ()
usage = do
    putStrLn $ usageInfo cliHeader cliOptions
    putStr helpText

parseArgs :: IO CliOptions
parseArgs = getArgs >>= parseOpts
