{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}

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

import Network.Google.CustomSearch
import qualified Network.Google.Billing as GB
import qualified Local.Config  as Cfg
import qualified Local.Message as Msg
import Local.Proxy

main :: IO ()
main = do
    config <- cmdArgs =<< (Cfg.customsearch <$> getProgName)
    if null (Cfg.fromDump config)
       then doSearches config
       else doFromDump config

doSearches :: Cfg.CustomSearch -> IO ()
doSearches config = do
    searchCfg  <- readSearchConfigFile
    when (null query) $ die Msg.emptyQuery
    mproxy <- fetchHttpConduitProxy
    -- first batch of results
    res <- doSearch searchCfg mproxy dump query start
    -- handle more results
    when (num > step && totalResults res > step) $
        if allowBilling searchCfg
           then do
               { let starts = drop 1 [ start, start + step .. start + num ]
               ; mapM_ (doSearch searchCfg mproxy dump query) starts
               }
           else do
               { cfile <- searchCfgFilePath
               ; die (Msg.dareNotExceedQuota cfile (Cfg.num config))
               }
  where
    query = unwords (Cfg.query config)
    num   = Cfg.num config
    start = Cfg.start config
    dump  = Cfg.dump config
    step = GB.maxResultsPerCustomSearch

doFromDump :: Cfg.CustomSearch -> IO ()
doFromDump config = do
    rawRes <- BL.readFile (Cfg.fromDump config)
    res    <- readResults query rawRes
    printResults res
    hPutStrLn stderr $ show (totalResults res) ++ " total results"
 where
    query = unwords (Cfg.query config)

-- | Perform a search, print results, dump as needed
doSearch :: SearchConfig -> Maybe Proxy -> Bool -> String -> Int -> IO GResults
doSearch searchCfg mproxy dump query start = do
    hPutStrLn stderr $ "Search starting from " ++ show start
    rawRes <- google searchCfg mproxy start query
    res    <- readResults query rawRes
    printResults res
    when dump $ do
         rfile <- dumpSearchResults query (show start) rawRes
         T.hPutStrLn stderr $ Msg.dumped rfile
    return res

google :: SearchConfig -> Maybe Proxy -> Int -> String -> IO BL.ByteString
google searchCfg mproxy start theQuery = flip catch reportException $ do
    request_ <- parseUrl . exportURL $ mkUrl searchCfg start theQuery
    let request = request_ { proxy = mproxy }
    withManager $ \mgr -> responseBody <$> httpLbs request mgr
  where
    reportException e = die (Msg.gotHttpException e)

-- | Read results, complaining if something goes wrong
readResults :: String        -- ^ query
            -> BL.ByteString -- ^ result
            -> IO GResults
readResults query searchRes =
    case decode searchRes of
        Nothing -> do
            { rfile <- dumpSearchResults query "problem" searchRes
            ; die $ Msg.didNotUnderstandJson rfile
            }
        Just j -> return j

printResults :: GResults -> IO ()
printResults = V.mapM_ (T.putStrLn . displayResult) . items

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

searchCfgFilePath :: IO FilePath
searchCfgFilePath = do
    appdir <- getAppUserDataDirectory "customsearch"
    return $ appdir </> "config" <.> "json"

readSearchConfigFile :: IO SearchConfig
readSearchConfigFile = do
    cfile  <- searchCfgFilePath
    exists <- doesFileExist cfile
    if exists
       then do
           let oops = Msg.searchConfigParseError cfile
           fromMaybeM (die oops) $ decode <$> BL.readFile cfile
       else do
           createDirectoryIfMissing False (takeDirectory cfile)
           die (Msg.searchConfigFileNotFound cfile)

displayResult :: GResult -> T.Text
displayResult gr =
    resUrl gr <> "\t" <> niceSnippet gr
  where
    niceSnippet = T.unwords . T.words . resSnippet

-- ---------------------------------------------------------------------
-- odds and ends
-- ---------------------------------------------------------------------

die :: T.Text -> IO a
die msg = T.hPutStrLn stderr msg >> exitWith (ExitFailure 1)

fromMaybeM :: Monad m => m a -> m (Maybe a) -> m a
fromMaybeM z job = maybe z return =<< job
