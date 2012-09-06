module Network.SearchEngine where

import qualified Data.Text as T
import qualified Data.Vector as V

class SearchEngine a where
    maxResultsPerSearch :: a -> Int
    shortName           :: a -> String

class Results a where
    items :: a -> V.Vector Result

data Result = Result
    { snippet :: T.Text
    , url     :: T.Text
    }
