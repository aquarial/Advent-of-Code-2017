{-# LANGUAGE OverloadedStrings #-}
module Day08 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text  (Parser)

import           Control.Monad
import           Data.List
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show

main = do
  input <- TIO.readFile "src/y2017/testinput08"
  case parse p "testinput08" input of
    Left  err   -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right betterInput -> do
      tprint $ partA betterInput
  input <- TIO.readFile "src/y2017/input08"
  case parse p "input08" input of
    Left  err   -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right betterInput -> do
      tprint $ partA betterInput
  return ()

data Ctrl = Inc | Dec

comparitor :: Text -> (Int -> Int -> Bool)
comparitor ts | ts == "==" = (==)
              | ts == "!=" = (/=)
              | ts == ">"  = (>)
              | ts == "<"  = (<)
              | ts == ">=" = (>=)
              | ts == "<=" = (<=)

p :: Parser [(Text, Int->Int, Text, (Int->Bool))]
p = line `sepBy` char '\n'

line = do
  name <- word
  string " "
  ctr <- (+) <$ string "inc" <|> (subtract) <$ string "dec"
  string " "
  size <- int
  string " if "
  other <- word
  string " "
  compar <- comparitor . T.pack <$> some (oneOf (['<','>','!','=']))
  string " "
  det <- int
  pure (name, ctr size, other, (\x -> x `compar` det))

word :: Parser Text
word = T.pack <$> some letterChar

int :: Parser Int
int = do
      change <- option id (negate <$ char '-')
      fromInteger . change <$> L.integer


tup1 (a,b,c) = a
tup2 (a,b,c) = b
tup3 (a,b,c) = c

partA xs = let (m, s) = walk xs (M.empty) 0
           in (maximum (M.elems m), s)

walk []                               m ma = (m, ma)
walk ((this, change, other, test):xs) m ma = walk xs (if test valother then M.insert this (change valthis) m else m) (maximum [ma, valthis, valother])
  where
    valthis :: Int
    valthis = case M.lookup this m of
            Just v -> v
            Nothing -> 0
    valother = case M.lookup other m of
            Just v -> v
            Nothing -> 0
