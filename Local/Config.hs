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
    , dump       :: Bool
    , query      :: [String]
    }
  deriving (Show, Data, Typeable)

customsearch :: FilePath -> CustomSearch
customsearch p = modes
    [ CustomSearch
        { num    = 10 &= typ "INT" &= help "Number of results to return"
        , dump   = False           &= help "Save machine readable results"
        , query  = []  &= args
        }
    ] &= program (takeFileName p)
      &= help (p ++ " " ++ showVersion version)
