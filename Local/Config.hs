{-# LANGUAGE DeriveDataTypeable #-}

-- | Cmdargs configuration
module Local.Config where

import Data.List
import Data.Version
import Prelude hiding ( catch )
import System.FilePath

import System.Console.CmdArgs

import Paths_customsearch

-- TODO: ability to specify search config files
-- TODO: ability to specify developer and cse key as flags
data CustomSearch = CustomSearch
    { num        :: Int
    , start      :: Int
    , query      :: [String]
    , dump       :: Bool
    , fromDump   :: FilePath
    }
  deriving (Show, Data, Typeable)

customsearch :: FilePath -> CustomSearch
customsearch p = modes
    [ CustomSearch
        { num    = 10 &= typ "INT" &= help "Number of results to return"
        , start  = 1  &= typ "INT" &= help "Starting from which result"
        , query  = []  &= args
        , dump     = False            &= help "Save machine readable results"
        , fromDump = "" &= typ "FILE" &= help "[DEBUG ONLY] Don't talk to Google. Read and print machine readable from file"
        }
    ] &= program (takeFileName p)
      &= help (p ++ " " ++ showVersion version)
