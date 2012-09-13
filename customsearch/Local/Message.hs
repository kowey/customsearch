{-# LANGUAGE OverloadedStrings #-}

-- | User-facing messages
module Local.Message where

import Data.Monoid
import Data.Text ( Text )
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Data.CaseInsensitive as CI
import Network.HTTP.Conduit
import Network.HTTP.Types

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
        [ ". The search engine complained:"
        , bullet $ "status code: " <> T.pack (show code)
        , bullet $ "message: "     <> T.decodeUtf8 msg
        ] ++ map fromHeader headers
    body err = ". Here's what went wrong: " <> T.pack (show err)
    --
    fromHeader (hdr, val) = bullet $ T.decodeUtf8 (CI.original hdr) <> ": " <> T.decodeUtf8 val
    bullet = ("- " <>)
