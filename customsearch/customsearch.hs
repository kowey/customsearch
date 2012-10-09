{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeFamilies, FlexibleContexts #-}

module Main where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
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
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text     as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T

import Data.Aeson
import Network.HTTP.Conduit
import Network.URL
import System.Console.CmdArgs
import System.Console.Haskeline hiding ( catch )
import qualified Data.Vector as V

import Network.Google.CustomSearch ( Google(..) )
import Network.Bing ( Bing(..) )
import Network.SearchEngine
import qualified Network.SearchEngine   as SE
import qualified Local.Config  as Cfg
import qualified Local.Message as Msg
import Local.Proxy

main :: IO ()
main = do
    config <- cmdArgs =<< (Cfg.customsearch <$> getProgName)
    case Cfg.engine config of
        Cfg.Google -> main' config Google
        Cfg.Bing   -> main' config Bing
  where
    main' config_ engine = do
        let ofile = Cfg.output config_
        unless (null ofile) $ do
            hasO <- doesFileExist ofile
            when hasO . die $ "The file " <> T.pack ofile <> " already exists.  I dare not overwrite it."
        if null (Cfg.fromDump config_)
            then do
                config  <- runInputT defaultSettings $ do
                    { hasT <- haveTerminalUI
                    ; if hasT
                         then augmentConfig engine config_
                         else return config_
                    }
                doSearches engine config
            else doFromDump engine config_

augmentConfig :: SearchEngineJson engine
              => engine
              -> Cfg.CustomSearch
              -> InputT IO Cfg.CustomSearch
augmentConfig engine config = do
    query <- ifMissing null Cfg.query
                 (singleton <$> (repeatWhile null $ getInputLine "What should I search for? "))
    outputToTerminal <- liftIO (hIsTerminalDevice stdout)
    ofile <- if outputToTerminal
                then ifMissing null Cfg.output $ repeatWhile null $
                         getInputLine "What file should I save the results to? "
                else return (Cfg.output config)
    num <- ifMissing isNothing Cfg.num $
               (Just <$> loopNum step "How many results should I return? ")
    return $ config
        { Cfg.query  = query
        , Cfg.output = ofile
        , Cfg.num    = num
        }
  where
    singleton = (: [])
    ifMissing :: (a -> Bool) -> (Cfg.CustomSearch -> a) -> InputT IO a -> InputT IO a
    ifMissing missing f q =
        if missing (f config) then q else return (f config)
    withSurrender job = do
        mstr <- job
        case mstr of
            Nothing -> liftIO $ die "OK, giving up"
            Just x  -> return x
    repeatWhile bad job = do
        str <- withSurrender job
        if bad str then repeatWhile bad job else return str
    loopNum dnum q = do
        numStr <- withSurrender $ getInputLineWithInitial q (show dnum, "")
        case maybeRead numStr of
            Nothing -> do
                outputStrLn "Sorry, I didn't understand your number."
                loopNum dnum q
            Just num -> return num
    step = SE.maxResultsPerSearch engine

-- for convenience in writing type signatures
class (SearchEngine e, FromJSON (SE.Config e), FromJSON (SE.Results e)) => SearchEngineJson e where

instance SearchEngineJson Bing
instance SearchEngineJson Google

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
    res <- doSearch config engine dump (mkSearch start)
    let thereIsMore = maybe True (> step) (totalResults res)
    -- handle more results
    case Cfg.num config of
        Nothing  -> return ()
        Just num -> when (num > step && thereIsMore) $
            if SE.allowMultiSearch searchCfg
               then do
                   { let starts = drop 1 [ start, start + step .. start + num ]
                   ; mapM_ (doSearch config engine dump . mkSearch) starts
                   }
               else do
                   { cfile <- searchCfgFilePath engine
                   ; die $ dareNotMultiSearch (SE.messages engine) cfile num
                   }
  where
    query = unwords (Cfg.query config)
    start = Cfg.start config
    dump  = Cfg.dump config
    step = SE.maxResultsPerSearch engine

doFromDump :: SearchEngineJson engine
           => engine
           -> Cfg.CustomSearch -> IO ()
doFromDump engine config = do
    rawRes <- BL.readFile (Cfg.fromDump config)
    res    <- readResults engine query rawRes
    printResults config res
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
         => Cfg.CustomSearch
         -> engine
         -> Bool -- ^ dump
         -> Search engine
         -> IO (SE.Results engine)
doSearch config engine dump search@(Search {sQuery, sStart}) = do
    hPutStrLn stderr $ "Search starting from " ++ show sStart
    rawRes <- connect search
    res    <- readResults engine sQuery rawRes
    printResults config res
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
             => Cfg.CustomSearch
             -> SE.Results engine
             -> IO ()
printResults config res = do
    unless (null ofile) $ do
        e <- doesFileExist ofile
        unless e $ B.writeFile ofile ""
    save . T.encodeUtf8 . T.unlines
         . V.toList . V.map displayResult
         . SE.items $ res
  where
    ofile = Cfg.output config
    save = if null ofile
              then B.putStr
              else B.appendFile (Cfg.output config)

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
           let oops = configParseError (messages engine) cfile
           fromMaybeM (die oops) $ decode <$> BL.readFile cfile
       else do
           createDirectoryIfMissing False (takeDirectory cfile)
           die $ configFileNotFound (messages engine) cfile

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

maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
    [(x, rest)] | all isSpace rest -> Just x
    _         -> Nothing
