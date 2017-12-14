{-# LANGUAGE OverloadedStrings #-}
module Y2017.Day14 where

import           Y2017.Day10           (hash)

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C8

import           Numeric
import           Data.Bits
import           Data.List
import           Data.Monoid

free :: ByteString -> Int
free = sum . map popCount . map tobin . C8.unpack
  where
    tobin :: Char -> Int
    tobin x = fst $ head $ readHex [x]

parta x = sum $ map (free . hash) [ x <> "-" <> t | t <- map (T.pack . show) [0..127]]
--map popCount $ mconcat

main :: IO ()
main = do tprint $ parta "ffayrhll"

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
