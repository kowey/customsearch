{-# LANGUAGE OverloadedStrings #-}

-- | Billing facts
module Network.Google.Billing where

import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as T

maxResultsPerCustomSearch :: Int
maxResultsPerCustomSearch = 10

freeCustomSearches :: Int
freeCustomSearches = 100

fees :: Text
fees = T.intercalate "\n"
    [ "Google currently allow " <> n <> " free searches a day, after which the"
    , "fee is $5 per 1000 searches (up to 10000 searches)"
    ]
  where
    n = T.pack (show freeCustomSearches)
