{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Bing web search
module Network.Bing where

import Control.Applicative
import Control.Monad
import GHC.Generics ( Generic )
import qualified Data.ByteString as B

import Data.Aeson
import Data.Aeson.Encode.Pretty
import qualified Data.Vector as V
import qualified Data.ByteString.Base64 as B64
import Network.HTTP.Types
import Network.URL

import qualified Network.SearchEngine as SE

import Data.Monoid
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

data Bing = Bing

instance SE.SearchEngine Bing where
    maxResultsPerSearch _ = 50
    shortName           _ = "bing"

-- | Application-specific configuration for this search engine
data SearchConfig = SearchConfig
    { apiKey       :: B.ByteString
    , allowBilling :: Bool
    }
  deriving (Generic)

instance FromJSON SearchConfig
instance ToJSON   SearchConfig

-- | Construct a URL and HTTP headers a custom search
mkRequest :: SearchConfig
          -> Int     -- ^ starting result
          -> String  -- ^ search term
          -> (URL, [Header])
mkRequest config start search =
    (url, headers)
  where
    url = URL
        { url_type   = Absolute $ Host (HTTP True)
              "api.datamarket.azure.com" Nothing
        , url_path   = "Data.ashx/Bing/SearchWeb/v1/Web"
        , url_params = [ ("Query", "'" ++ search ++ "'")
                       , ("$format", "json")
                       , ("$skip", show (start - 1))
                       ]
        }
    k64 = B64.encode $ B.concat [ apiKey config, ":", apiKey config ]
    headers =
        [( "Authorization", "Basic " `B.append` k64)
        ]

-- ---------------------------------------------------------------------
--
-- ---------------------------------------------------------------------

-- | Search results and any relevant metadata returned from them
--
--   (Note, currently very sparse and limited to the bits I'm using)
data Results = Results
    { results :: V.Vector BingResult
    }
  deriving Generic

instance FromJSON Results where
    parseJSON (Object v) =
        Results <$> (v .: "d" >>= (.: "results"))
    parseJSON _ = mzero

instance SE.Results Results where
    items = V.map fromBingResult . results

-- | A single hit in the search
newtype BingResult = BingResult { fromBingResult :: SE.Result }

instance FromJSON BingResult where
    parseJSON (Object v) =
        BingResult <$> res
      where
        res = SE.Result <$> v .: "Description"
                        <*> v .: "Url"
    parseJSON _ = mzero

-- ---------------------------------------------------------------------
--
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
    [ "1. Sign up for the Windows Azure marketplace at"
    , "   <http://datamarket.azure.com/>"
    , ""
    , "2. Find your Primary Account Key at"
    , "   <https://datamarket.azure.com/account>"
    , ""
    , "3. Copy this text into a text editor and replace the API keys"
    ,     TL.toStrict . TL.decodeUtf8 $ encodePretty fakeConfig
    , ""
    , "4. Save the results in " <> T.pack cfile
    ]
  where
    fakeConfig = SearchConfig
        { apiKey       = "ABCDEFG0123456890ABCDEFGHIJKLMOPQRSTUV="
        , allowBilling = False
        }

dareNotExceedQuota :: FilePath -> Int -> Text
dareNotExceedQuota cfile num = unlines_
    [ "You've asked for " <> tshow num <> " results, but at the time of"
    , "this writing, Microsoft only allow " <> tshow rps <> " results per search."
    , ""
    , "I can automatically do the " <> tshow more <> " more searches needed to get the"
    , "results; but I want to make sure you are happy with the potential fees first."
    , fees
    , ""
    , "If this sounds OK, please"
    , ""
    , "1. Set \"allowBilling\" to true in " <> T.pack cfile
    , "2. Do this search again"
    ]
  where
    rps   = SE.maxResultsPerSearch Bing
    more  = (num `divUp` rps) - 1
    x `divUp` y = case x `quotRem` y of
                      (q, 0) -> q
                      (q, _) -> q + 1

fees :: Text
fees = T.intercalate "\n"
    [ "Microsoft currently allow " <> n <> " free searches a month"
    , ""
    , "If you need to raise this limit, have a look at"
    , "http://datamarket.azure.com/dataset/8818F55E-2FE5-4CE3-A617-0B8BA8419F65"
    , "for the current prices."
    ]
  where
    n = T.pack (show freeMonthly)

freeMonthly :: Int
freeMonthly = 5000

tshow :: Show a => a -> Text
tshow = T.pack . show

-- | No trailing newline at the end
unlines_ :: [Text] -> Text
unlines_ = T.intercalate "\n"
