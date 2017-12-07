{-# LANGUAGE OverloadedStrings #-}
module Day06 where

import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text  (Parser)

import           Data.List
import qualified Data.Map.Strict       as M
import           Data.Maybe            (catMaybes)
import qualified Data.Set              as S

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show

main = do
  input <- TIO.readFile "src/y2017/input06"
  tprint $ partstr $ T.splitOn "\n" input
  case parse p "input06" input of
    Left  err   -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right betterInput -> do
      tprint $ part betterInput
  return ()

p :: Parser [[Int]]
p = (int `sepBy` space) `sepBy` char '\n'
  where
    int = do
      change <- negate <$ char '-' <|> pure id
      fromInteger . change <$> L.integer

part xs = xs


partstr xs = xs

replace :: Int -> a -> [a] -> [a]
replace i e []     = []
replace 0 e (x:xs) = e:xs
replace i e (x:xs) = x:replace (i-1) e xs
