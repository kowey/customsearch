{-# LANGUAGE OverloadedStrings #-}

-- | User-facing messages
module Local.Message where

import Data.Monoid
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import Data.Aeson.Encode.Pretty
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Conduit
import Network.HTTP.Types

import qualified Network.Google.Billing as GB
import Network.Google.CustomSearch

emptyQuery :: Text
emptyQuery =
    "Please give me something to search for"

dumped :: FilePath -> Text
dumped rfile =
    "Machine-readable search results saved in " <> T.pack rfile

didNotUnderstandJson :: FilePath -> Text
didNotUnderstandJson rfile = T.unwords
    [ "Sorry, I didn't understand the search results returned by Google,"
    , "but I've saved them in"
    , T.pack rfile
    ]

gotHttpException :: HttpException -> Text
gotHttpException e =
    "Sorry, I had some trouble performing your search" <> body e
  where
    body (StatusCodeException (Status code msg) headers) = T.intercalate "\n" $
        [ ". Google complained:"
        , bullet $ "status code: " <> T.pack (show code)
        , bullet $ "message: "     <> T.decodeUtf8 msg
        ] ++ map fromHeader headers
    body err = ". Here's what went wrong: " <> T.pack (show err)
    --
    fromHeader (hdr, val) = bullet $ T.decodeUtf8 (CI.original hdr) <> ": " <> T.decodeUtf8 val
    bullet = ("- " <>)

-- ---------------------------------------------------------------------
-- * Setup and search configuration
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
