{-# LANGUAGE OverloadedStrings #-}
module Day07 where

import  Data.Text             (Text)
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
  input <- TIO.readFile "src/y2017/input07"
  case parse p "input07" input of
    Left  err   -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right betterInput -> do
      tprint $ part betterInput
  return ()


fileName :: Parser (String, Int, [String])
fileName = do
  name <- some $noneOf [' ']
  char ' '
  char '('
  size <- int
  char ')'
  deps <- supports <|> pure []
  return (name, size, deps)

p :: Parser [(String, Int, [String])]
p = fileName `sepBy` char '\n'

supports :: Parser [String]
supports = do
      string (" -> " :: String)
      (sepBy word (string ", "))
  where
    word :: Parser String
    word =  some $ noneOf (", \n" :: String)

int = do
      change <- negate <$ char '-' <|> pure id
      fromInteger . change <$> L.integer

part xs = filter (appearsInSupports xs) xs

appearsInSupports xs (name,_,_) = all (\(_,_,c) -> notElem name c) xs

partstr xs = xs

replace :: Int -> a -> [a] -> [a]
replace i e []     = []
replace 0 e (x:xs) = e:xs
replace i e (x:xs) = x:replace (i-1) e xs
