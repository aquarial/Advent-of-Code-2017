{-# LANGUAGE OverloadedStrings #-}
module Y2017.Day17 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text  (Parser)

import           Data.List
import qualified Data.Map.Strict       as M
import qualified Data.HashSet          as S
import qualified Data.Graph            as G

parta dist = head $ head $ drop 2017 $ walk 0 dist []

walk n dx xs = step : walk (n+1) dx step
  where
    step = drop dxm xs ++ take dxm xs ++ [n]
    dxm = dx `mod` (if n == 0 then 1 else n)

main :: IO ()
main = do
  tprint $ parta 394

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
