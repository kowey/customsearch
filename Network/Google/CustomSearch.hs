{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric, TypeFamilies, FlexibleInstances #-}

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
import Network.HTTP.Types
import Network.URL
import qualified Data.Vector as V
import qualified Network.SearchEngine as SE
import qualified Network.Google.Billing as GB

data Google = Google

instance SE.SearchEngine Google where
    data Config  Google = Config  GoogleConfig
    data Results Google = Results GoogleResults

    maxResultsPerSearch _ = GB.maxResultsPerCustomSearch
    shortName _           = "google-cse"
    mkRequest (Config c)        = mkRequest c
    allowMultiSearch (Config c) = allowBilling c

    items (Results r) = V.map fromGoogleResult (items r)
    totalResults (Results r) = Just (totalResults r)

    messages _ = SE.Messages
        { SE.configFileNotFound = configFileNotFound
        , SE.configParseError   = configParseError
        , SE.configInstructions = configInstructions
        , SE.dareNotMultiSearch = dareNotMultiSearch
        }

instance FromJSON (SE.Config Google) where
   parseJSON j = Config <$> parseJSON j

instance FromJSON (SE.Results Google) where
   parseJSON j = Results <$> parseJSON j

-- ---------------------------------------------------------------------
-- configuration
-- ---------------------------------------------------------------------

-- | Application-specific configuration for custom search
data GoogleConfig = GoogleConfig
    { searchEngine   :: String -- ^ Custom search engine, see http://www.google.com/cse
    , devKey         :: String -- ^ Your API developer key, see http://code.google.com/apis/console
    , allowBilling   :: Bool
    }
  deriving (Generic)

instance FromJSON GoogleConfig
instance ToJSON   GoogleConfig

-- ---------------------------------------------------------------------
-- requests
-- ---------------------------------------------------------------------

-- | Construct a URL for a custom search
mkRequest :: GoogleConfig
          -> Int     -- ^ starting result
          -> String  -- ^ search term
          -> (URL, [Header])
mkRequest config start search =
    (url, headers)
  where
    url = URL
        { url_type   = Absolute google
        , url_path   = "customsearch/v1"
        , url_params = [ ("q", search)
                       , ("alt", "json")
                       , ("cx",  searchEngine config)
                       , ("key", devKey config)
                       , ("start", show start)
                       ]
        }
    google = Host (HTTP True) "www.googleapis.com" Nothing
    headers = []

-- ---------------------------------------------------------------------
-- results
-- ---------------------------------------------------------------------

-- | Search results and any relevant metadata returned from them
--
--   (Note, currently very sparse and limited to the bits I'm using)
data GoogleResults = GoogleResults
    { items        :: V.Vector GoogleResult
    , totalResults :: Int
    }
  deriving Generic

instance FromJSON GoogleResults where
    parseJSON (Object v) =
        GoogleResults <$> (v .: "items")
                 <*> (parseSI =<< (v .: "searchInformation"))
      where
        parseSI (Object sv) = do
            tr <- sv .: "totalResults" -- sigh, not actually a number
            case T.decimal tr of
                Right (n,"") -> return n
                _            -> mzero
        parseSI _ = mzero
    parseJSON _ = mzero

-- | A single hit in the search
--
--   (Note, currently very sparse and limited to the bits I'm using)
newtype GoogleResult = GoogleResult { fromGoogleResult :: SE.Result }

instance FromJSON GoogleResult where
    parseJSON (Object v) = GoogleResult <$>
        (SE.Result <$> v .: "snippet"
                   <*> v .: "link"
        )
    parseJSON _ = mzero

-- ---------------------------------------------------------------------
-- * Messages
-- ---------------------------------------------------------------------

configFileNotFound :: FilePath -> T.Text
configFileNotFound cfile = unlines_
    [ "Please create the configuration file " <> T.pack cfile <> ":"
    , ""
    , configInstructions cfile
    ]

configParseError :: FilePath -> T.Text
configParseError cfile = unlines_
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
    fakeConfig = GoogleConfig
        { searchEngine = "012345678901234567890:thisisafake"
        , devKey       = "saonetuhash38hsreaochusarchsSR3bzllgggx"
        , allowBilling = False
        }

dareNotMultiSearch :: FilePath -> Int -> Text
dareNotMultiSearch cfile num = unlines_
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
