{-# LANGUAGE OverloadedStrings #-}
module Day11 where


import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B

import qualified Data.Bits             as Bits
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text  (Parser)

import           Data.List
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S


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

partA = id

test input = case parse p "test" input of
               Left  err -> TIO.putStr $ T.pack $ parseErrorPretty err
               Right bi  -> tprint $ partA bi

main :: IO ()
main = do
  input <- TIO.readFile "src/y2017/input11"
  case parse p "input11" input of
    Left  err   -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right betterInput -> do
      tprint $ partA betterInput
  return ()


tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
