{-# LANGUAGE TypeFamilies #-}

module Network.SearchEngine where

import qualified Data.Text as T
import qualified Data.Vector as V

import Network.HTTP.Types
import Network.URL

class SearchEngine a where
    -- I think I'd just rather have a type synonym here, but I run into injectivity stuff
    data Config a  :: *
    maxResultsPerSearch :: a -> Int
    shortName           :: a -> String
    mkRequest           :: Config a -> Int -> String -> (URL, [Header])
    allowMultiSearch    :: Config a -> Bool -- ^ user explicitly allows us to do multishot searches

class Results a where
    items :: a -> V.Vector Result

data Result = Result
    { snippet :: T.Text
    , url     :: T.Text
    }
