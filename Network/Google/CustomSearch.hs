{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Google Custom Search Engine
module Network.Google.CustomSearch where

import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Text ( Text )
import GHC.Generics ( Generic )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Read as T

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Network.URL
import qualified Data.Vector as V
import qualified Network.SearchEngine as SE
import qualified Network.Google.Billing as GB

data GoogleCSE = GoogleCSE

instance SE.SearchEngine GoogleCSE where
    maxResultsPerSearch _ = GB.maxResultsPerCustomSearch
    shortName _           = "google-cse"

-- | Application-specific configuration for custom search
data SearchConfig = SearchConfig
    { searchEngine   :: String -- ^ Custom search engine, see http://www.google.com/cse
    , devKey         :: String -- ^ Your API developer key, see http://code.google.com/apis/console
    , allowBilling   :: Bool
    }
  deriving (Generic)

instance FromJSON SearchConfig
instance ToJSON   SearchConfig

-- | Construct a URL for a custom search
mkUrl :: SearchConfig
      -> Int     -- ^ starting result
      -> String  -- ^ search term
      -> URL
mkUrl config start search = URL
    { url_type   = Absolute google
    , url_path   = "customsearch/v1"
    , url_params = [ ("q", search)
                   , ("alt", "json")
                   , ("cx",  searchEngine config)
                   , ("key", devKey config)
                   , ("start", show start)
                   ]
    }
  where
    google = Host (HTTP True) "www.googleapis.com" Nothing

-- ---------------------------------------------------------------------
--
-- ---------------------------------------------------------------------

-- | Search results and any relevant metadata returned from them
--
--   (Note, currently very sparse and limited to the bits I'm using)
data GResults = GResults
    { items        :: V.Vector GResult
    , totalResults :: Int
    }
  deriving Generic

instance FromJSON GResults where
    parseJSON (Object v) =
        GResults <$> (v .: "items")
                 <*> (parseSI =<< (v .: "searchInformation"))
      where
        parseSI (Object sv) = do
            tr <- sv .: "totalResults" -- sigh, not actually a number
            case T.decimal tr of
                Right (n,"") -> return n
                _            -> mzero
        parseSI _ = mzero
    parseJSON _ = mzero

instance SE.Results GResults where
    items = V.map fromGResult . items

-- | A single hit in the search
--
--   (Note, currently very sparse and limited to the bits I'm using)
newtype GResult = GResult { fromGResult :: SE.Result }

instance FromJSON GResult where
    parseJSON (Object v) = GResult <$>
        (SE.Result <$> v .: "snippet"
                   <*> v .: "link"
        )
    parseJSON _ = mzero

-- ---------------------------------------------------------------------
-- * Messages
-- ---------------------------------------------------------------------

searchConfigFileNotFound :: FilePath -> T.Text
searchConfigFileNotFound cfile = unlines_
    [ "Please create the configuration file " <> T.pack cfile <> ":"
    , ""
    , configInstructions cfile
    ]

searchConfigParseError :: FilePath -> T.Text
searchConfigParseError cfile = unlines_
    [ "Sorry! I didn't understand the configuration file " <> T.pack cfile
    , "In case it helps, here are the instructions for creating that file again:"
    , ""
    , configInstructions cfile
    ]

configInstructions :: FilePath -> T.Text
configInstructions cfile = unlines_
    [ "1. Get a developer API key from Google <http://code.google.com/apis/console>"
    , ""
    , "2. Create a custom search engine <http://www.google.com/cse>"
    , "   HINT: after you create the engine, you can go back and configure it to"
    , "   search the whole web, rather than just a few sites"
    , ""
    , "3. Copy this text into a text editor and replace the search engine and dev keys"
    ,     TL.toStrict . TL.decodeUtf8 $ encodePretty fakeConfig
    , ""
    , "4. Save the results in " <> T.pack cfile
    ]
  where
    fakeConfig = SearchConfig
        { searchEngine = "012345678901234567890:thisisafake"
        , devKey       = "saonetuhash38hsreaochusarchsSR3bzllgggx"
        , allowBilling = False
        }

dareNotExceedQuota :: FilePath -> Int -> Text
dareNotExceedQuota cfile num = unlines_
    [ "You've asked for " <> tshow num <> " results, but at the time of"
    , "this writing, Google only allow " <> tshow rps <> " results per search."
    , ""
    , "I can automatically do the " <> tshow more <> " more searches needed to get the"
    , "results; but I want to make sure you are happy with the potential fees first."
    , GB.fees
    , ""
    , "If this sounds OK, please"
    , ""
    , "1. Have Google enable billing for your API key"
    , "   http://code.google.com/apis/console/?api=customsearch"
    , "2. Set \"allowBilling\" to true in " <> T.pack cfile
    , "3. Do this search again"
    ]
  where
    rps   = GB.maxResultsPerCustomSearch
    more  = (num `divUp` rps) - 1
    x `divUp` y = case x `quotRem` y of
                      (q, 0) -> q
                      (q, _) -> q + 1

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | No trailing newline at the end
unlines_ :: [Text] -> Text
unlines_ = T.intercalate "\n"
