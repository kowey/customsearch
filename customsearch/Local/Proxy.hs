{-# LANGUAGE OverloadedStrings, ViewPatterns #-}

module Local.Proxy where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Read as T

import Network.HTTP.Proxy
import qualified Network.HTTP.Conduit as PC

-- | Bit of duct-tape to use HTTP's proxy detection and pass it to
--   http-conduit
fetchHttpConduitProxy :: IO (Maybe PC.Proxy)
fetchHttpConduitProxy = convertProxy =<< fetchProxy True

convertProxy :: Proxy -> IO (Maybe PC.Proxy)
convertProxy NoProxy        = return Nothing
convertProxy (Proxy prox _) = do
    if T.null pstr
       then returnProxy 80
       else case T.decimal pstr of
                Right (p, "") -> returnProxy p
                Left err      -> fail $ oops (Just err)
                Right (_, _)  -> fail $ oops Nothing
  where
    returnProxy  = return . Just . PC.Proxy (T.encodeUtf8 host)
    (host, pstr) = splitHostPort (dropHttp (T.pack prox))
    --
    dropHttp (T.stripPrefix "http://" -> Just hp) = hp
    dropHttp s = s
    splitHostPort hp =
        case T.breakOnEnd ":" hp of
            ("",h)           -> (h, "")
            (T.init -> h, p) -> (h, p)
    oops merr =
        "Sorry, I couldn't understand the port number ("
        ++ T.unpack pstr ++ ") in your proxy settings (" ++ prox ++ ") "
        ++ maybe "" (":" ++) merr
