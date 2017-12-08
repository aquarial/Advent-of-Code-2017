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
  input <- TIO.readFile "src/y2017/input08"
  case parse p "testinput08" input of
    Left  err   -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right betterInput -> do
      tprint $ partA betterInput
  case parse p "input08" input of
    Left  err   -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right betterInput -> do
      tprint $ partA betterInput
  return ()

p :: Parser [()]
p = line `sepBy` char '\n'

line = do
  name <- word
  string " ("
  size <- int
  string ")"
  --deps <- option []
  pure ()

word :: Parser Text
word = T.pack <$> some letterChar

int :: Parser Int
int = do
      change <- option id (negate <$ char '-')
      fromInteger . change <$> L.integer


tup1 (a,b,c) = a
tup2 (a,b,c) = b
tup3 (a,b,c) = c

partA xs = xs
