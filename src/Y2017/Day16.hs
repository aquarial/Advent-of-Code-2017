{-# LANGUAGE OverloadedStrings #-}
module Y2017.Day16 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Data.List
import qualified Data.Map.Strict       as M
import qualified Data.HashSet          as S
import qualified Data.Graph            as G

data Move = Spin Int | Exchange Int Int | Partner Char Char

parta :: [Move] -> [Char]
parta moves = dance moves ['a'..'p']

partb :: [Move] -> [Char]
partb moves = iterate (dance moves) ['a'..'p'] !! ((10^9) `mod` loop)
  where loop = findloop $ iterate (dance moves) ['a'..'p']

findloop xs = walk 0 S.empty xs
  where walk n s (x:xs) = if S.member x s then n else walk (n+1) (S.insert x s) xs

dance :: Foldable t => t Move -> [Char] -> [Char]
dance moves ds = foldl' domove ds moves

domove ds (Spin i) = reverse $ rotateL i $ reverse ds
domove ds (Exchange a b) = replace b (ds!!a) $ replace a (ds!!b) ds
domove ds (Partner pa pb) = case (elemIndex pa ds, elemIndex pb ds) of
                              (Just ax, Just bx) -> domove ds (Exchange ax bx)
                              _                  -> ds

rotateL :: Int -> [a] -> [a]
rotateL dx xs = drop dx xs ++ take dx xs

replace :: Int -> a -> [a] -> [a]
replace i x []     = []
replace 0 x (y:ys) = x:ys
replace i x (y:ys) = y : replace (i-1) x ys


type Parser = Parsec Void Text

p :: Parser [Move]
p = pmove `sepBy` char ','

pmove :: Parser Move
pmove = Spin <$> (char 's' *> int) <|>
        Exchange <$> (char 'x' *> int) <*> (char '/' *> int) <|>
        Partner <$> (char 'p' *> anyChar) <*> (char '/' *> anyChar)


word :: Parser Text
word = T.pack <$> some letterChar

int :: Parser Int
int = do change <- option id (negate <$ char '-')
         fromInteger . change <$> L.decimal


main :: IO ()
main = do
  input <- TIO.readFile "src/Y2017/input16"
  case parse p "input16" input of
    Left err -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right bi -> do
      TIO.putStrLn . T.pack $ parta bi
      TIO.putStrLn . T.pack $ partb bi



tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
