{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.Monoid
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text     as T
import qualified Data.Text.IO  as T
import qualified Data.Text.Lazy  as TL
import qualified Data.Text.Lazy.Encoding  as TL

import Data.Aeson
import Data.Aeson.Encode.Pretty
import Network.HTTP.Conduit
import Network.URL
import qualified Data.Vector as V

import Network.Google.CustomSearch

deriving instance Show SearchConfig

main :: IO ()
main = do
    cfg   <- readSearchConfigFile
    query <- unwords <$> getArgs
    searchRes  <- simpleHttp $ exportURL (mkUrl cfg query)
    tdir       <- getTemporaryDirectory
    case decode searchRes of
        Nothing -> do
            let rfile = tdir </> mkSearchFileName query <.> "json"
            BL.writeFile rfile searchRes
            hPutStrLn stderr $ unwords
                [ "Sorry, I didn't understand the search results returned by Google,"
                , "but I've saved them in"
                , rfile
                ]
            exitWith (ExitFailure 1)
        Just j ->
            V.mapM_ (T.putStrLn . displayResult) (fromGResults j)
  where
    mkSearchFileName = intercalate "-"
                     . map (filter isAlpha)
                     . words

readSearchConfigFile :: IO SearchConfig
readSearchConfigFile = do
    appdir <- getAppUserDataDirectory "customsearch"
    let cfile = appdir </> "config" <.> "json"
    exists <- doesFileExist cfile
    if exists
       then do
           mcfg <- decode <$> BL.readFile cfile
           case mcfg of
               Nothing -> do
                   T.hPutStr stderr $ T.unlines
                       [ "Sorry! I didn't understand the configuration file " <> T.pack cfile
                       , "In case it helps, here are the instructions for creating that file again:"
                       , ""
                       , configInstructions cfile
                       ]
                   exitWith (ExitFailure 1)
               Just cfg -> return cfg
       else do
           createDirectoryIfMissing False appdir
           T.hPutStr stderr $ T.unlines
               [ "Please create the configuration file " <> T.pack cfile <> ":"
               , ""
               , configInstructions cfile
               ]
           exitWith (ExitFailure 1)

configInstructions :: FilePath -> T.Text
configInstructions cfile = T.intercalate "\n"
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
        }


displayResult :: GResult -> T.Text
displayResult gr =
    resUrl gr <> "\t" <> niceSnippet gr
  where
    niceSnippet = T.unwords . T.words . resSnippet
