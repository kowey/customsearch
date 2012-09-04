{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Maybe
import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text   as T
import qualified Data.Vector as V

main = do
    [x] <- getArgs
    j   <- blop x =<< (decode <$> BL.readFile x)
    flip V.mapM_ (fromGResults j) $ print
  where
    blop x = maybe (fail ("Ack! Failed to parse JSON in " ++ x)) return

-- ---------------------------------------------------------------------
--
-- ---------------------------------------------------------------------

newtype GResults = GResults { fromGResults :: V.Vector GResult }

instance FromJSON GResults where
    parseJSON (Object v) = GResults <$> v .: "items"
    parseJSON _ = mzero

data GResult = GResult
    { resSnippet :: T.Text
    , resUrl     :: T.Text
    }
  deriving Show

instance FromJSON GResult where
    parseJSON (Object v) =
        GResult <$> v .: "snippet"
                <*> v .: "link"
