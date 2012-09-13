{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Main where

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import Prelude hiding ( catch )
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text     as T
import qualified Data.Text.IO  as T

import Data.Aeson
import Network.HTTP.Conduit
import Network.URL
import System.Console.CmdArgs
import qualified Data.Vector as V

-- import Network.Google.CustomSearch
import Network.Bing
import Network.SearchEngine   ( SearchEngine )
import qualified Network.SearchEngine   as SE
import qualified Local.Config  as Cfg
import qualified Local.Message as Msg
import qualified Network.Bing  as Msg -- TODO generalise
import Local.Proxy

main :: IO ()
main = do
    config <- cmdArgs =<< (Cfg.customsearch <$> getProgName)
    if null (Cfg.fromDump config)
       then doSearches Bing config
       else doFromDump Bing config

-- for convenience in writing type signatures
class (SearchEngine e, FromJSON (SE.Config e), FromJSON (SE.Results e)) => SearchEngineJson e where

instance SearchEngineJson Bing

doSearches :: SearchEngineJson engine
           => engine
           -> Cfg.CustomSearch
           -> IO ()
doSearches engine config = do
    searchCfg  <- readSearchConfigFile engine
    when (null query) $ die Msg.emptyQuery
    mproxy <- fetchHttpConduitProxy
    -- first batch of results
    let mkSearch = Search engine searchCfg mproxy query
    res <- doSearch engine dump (mkSearch start)
    -- handle more results
    when (num > step) $ -- TODO && totalResults res > step) $
        if SE.allowMultiSearch searchCfg
           then do
               { let starts = drop 1 [ start, start + step .. start + num ]
               ; mapM_ (doSearch engine dump . mkSearch) starts
               }
           else do
               { cfile <- searchCfgFilePath engine
               ; die (Msg.dareNotExceedQuota cfile (Cfg.num config))
               }
  where
    query = unwords (Cfg.query config)
    num   = Cfg.num config
    start = Cfg.start config
    dump  = Cfg.dump config
    step = SE.maxResultsPerSearch engine

doFromDump :: SearchEngineJson engine
           => engine
           -> Cfg.CustomSearch -> IO ()
doFromDump engine config = do
    rawRes <- BL.readFile (Cfg.fromDump config)
    res    <- readResults engine query rawRes
    printResults res
    -- hPutStrLn stderr $ show (totalResults res) ++ " total results"
 where
    query = unwords (Cfg.query config)

data SearchEngine engine => Search engine = Search
    { sEngine :: engine
    , sConfig :: SE.Config engine
    , sProxy  :: Maybe Proxy
    , sQuery  :: String
    , sStart  :: Int
    }

-- | Perform a search, print results, dump as needed
doSearch :: SearchEngineJson engine
         => engine
         -> Bool -- ^ dump
         -> Search engine
         -> IO (SE.Results engine)
doSearch engine dump search@(Search {sQuery, sStart}) = do
    hPutStrLn stderr $ "Search starting from " ++ show sStart
    rawRes <- connect search
    res    <- readResults engine sQuery rawRes
    printResults res
    when dump $ do
         rfile <- dumpSearchResults sQuery (show sStart) rawRes
         T.hPutStrLn stderr $ Msg.dumped rfile
    return res

connect :: SearchEngine engine
        => Search engine
        -> IO BL.ByteString
connect (Search {sConfig, sProxy, sQuery, sStart}) = flip catch reportException $ do
    request_ <- parseUrl . exportURL $ url
    let request = request_
            { proxy          = sProxy
            , requestHeaders = requestHeaders request_ ++ headers
            }
    withManager $ \mgr -> responseBody <$> httpLbs request mgr
  where
    reportException e = die (Msg.gotHttpException e)
    (url, headers)    = SE.mkRequest sConfig sStart sQuery

-- | Read results, complaining if something goes wrong
readResults :: SearchEngineJson engine
            => engine
            -> String        -- ^ query
            -> BL.ByteString -- ^ result
            -> IO (SE.Results engine)
readResults _ query searchRes =
    case decode searchRes of
        Nothing -> do
            { rfile <- dumpSearchResults query "problem" searchRes
            ; die $ Msg.didNotUnderstandJson rfile
            }
        Just j -> return j

printResults :: SearchEngineJson engine
             => SE.Results engine
             -> IO ()
printResults = V.mapM_ (T.putStrLn . displayResult) . SE.items

-- | Returns file name for dumped results
dumpSearchResults :: String -> String -> BL.ByteString -> IO FilePath
dumpSearchResults query suffix res = do
    tdir <- getTemporaryDirectory
    let rfile = tdir </> mkSearchFileName query <.> suffix <.> "json"
    BL.writeFile rfile res
    return rfile
  where
    mkSearchFileName = intercalate "-"
                     . map (filter isAlpha)
                     . words

searchCfgFilePath :: SearchEngineJson e => e -> IO FilePath
searchCfgFilePath engine = do
    appdir <- getAppUserDataDirectory "customsearch"
    return $ appdir </> SE.shortName engine <.> "json"

readSearchConfigFile :: SearchEngineJson e => e -> IO (SE.Config e)
readSearchConfigFile engine = do
    cfile  <- searchCfgFilePath engine
    exists <- doesFileExist cfile
    if exists
       then do
           let oops = Msg.searchConfigParseError cfile
           fromMaybeM (die oops) $ decode <$> BL.readFile cfile
       else do
           createDirectoryIfMissing False (takeDirectory cfile)
           die (Msg.searchConfigFileNotFound cfile)

displayResult :: SE.Result -> T.Text
displayResult gr =
    SE.url gr <> "\t" <> niceSnippet gr
  where
    niceSnippet = T.unwords . T.words . SE.snippet

-- ---------------------------------------------------------------------
-- odds and ends
-- ---------------------------------------------------------------------

die :: T.Text -> IO a
die msg = T.hPutStrLn stderr msg >> exitWith (ExitFailure 1)

fromMaybeM :: Monad m => m a -> m (Maybe a) -> m a
fromMaybeM z job = maybe z return =<< job
