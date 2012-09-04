{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Google Custom Search Engine
module Network.Google.CustomSearch where

import Control.Applicative
import Control.Monad
import GHC.Generics ( Generic )
import qualified Data.Text   as T

import Data.Aeson
import Network.URL
import qualified Data.Aeson.Generic as AG
import qualified Data.Vector as V

-- | Application-specific configuration for custom search
data SearchConfig = SearchConfig
    { searchEngine :: String -- ^ Custom search engine, see http://www.google.com/cse
    , devKey       :: String -- ^ Your API developer key, see http://code.google.com/apis/console
    }
  deriving (Generic)

instance FromJSON SearchConfig
instance ToJSON   SearchConfig

-- | Construct a URL for a custom search
mkUrl :: SearchConfig -> String -> URL
mkUrl config search = URL
    { url_type   = Absolute host
    , url_path   = "customsearch/v1"
    , url_params = [ ("q", search)
                   , ("alt", "json")
                   , ("cx",  searchEngine config)
                   , ("key", devKey config)
                   ]
    }
  where
    host = Host (HTTP True) "www.googleapis.com" Nothing

-- ---------------------------------------------------------------------
--
-- ---------------------------------------------------------------------

-- | Search results and any relevant metadata returned from them
--
--   (Note, currently very sparse and limited to the bits I'm using)
newtype GResults = GResults { fromGResults :: V.Vector GResult }

instance FromJSON GResults where
    parseJSON (Object v) = GResults <$> v .: "items"
    parseJSON _ = mzero

-- | A single hit in the search
--
--   (Note, currently very sparse and limited to the bits I'm using)
data GResult = GResult
    { resSnippet :: T.Text
    , resUrl     :: T.Text
    }
  deriving Show

instance FromJSON GResult where
    parseJSON (Object v) =
        GResult <$> v .: "snippet"
                <*> v .: "link"
