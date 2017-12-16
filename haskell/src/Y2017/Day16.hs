{-# LANGUAGE OverloadedStrings #-}
module Y2017.Day16 where

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

data Move = Spin Int | Exchange Int Int | Partner Char Char

parta :: [Move] -> [Char]
parta = foldl' domove dancers
  where
    dancers = ['a'..'p']

domove ds (Spin i) = reverse $ rotateL i $ reverse ds
domove ds (Exchange a b) = replace b (ds!!a) $ replace a (ds!!b) ds
domove ds (Partner pa pb) = case (elemIndex pa ds, elemIndex pb ds) of
                              (Just ax, Just bx) -> domove ds (Exchange ax bx)
                              _                  -> ds

rotateL dx xs = drop dx xs ++ take dx xs

replace i x []     = []
replace 0 x (a:as) = x:as
replace i x (a:as) = a : replace (i-1) x as

p :: Parser [Move]
p = pmove `sepBy` char ','

pmove = Spin <$> (char 's' *> int) <|>
        Exchange <$> (char 'x' *> int) <*> (char '/' *> int) <|>
        Partner <$> (char 'p' *> anyChar) <*> (char '/' *> anyChar)


word :: Parser Text
word = T.pack <$> some letterChar

int :: Parser Int
int = do change <- option id (negate <$ char '-')
         fromInteger . change <$> L.integer


main :: IO ()
main = do
  input <- TIO.readFile "src/Y2017/input16"
  case parse p "input16" input of
    Left err -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right bi -> do
      tprint $ parta bi

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
