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


p :: Parser [Move]
p = (move <$> word)`sepBy` char ','

data Move = S | SW | NW | N | NE | SE

move :: Text -> Move
move "se" = SE
move "s" = S
move "sw" = SW
move "ne" = NE
move "nw" = NW
move "n" = N

word :: Parser Text
word = T.pack <$> some letterChar

int :: Parser Int
int = do
      change <- option id (negate <$ char '-')
      fromInteger . change <$> L.integer

partA = maximum . map dist . walk (0,0)


walk (a,b) (S:xs) = (a,b):walk (a,b+1) xs
walk (a,b) (SE:xs) = (a,b):walk (a+1,b) xs
walk (a,b) (SW:xs) = (a,b):walk (a-1,b+1) xs
walk (a,b) (N:xs) =  (a,b):walk (a,b-1) xs
walk (a,b) (NE:xs) = (a,b):walk (a+1,b-1) xs
walk (a,b) (NW:xs) = (a,b):walk (a-1,b) xs
walk  z      []      = [z]

dist (a,b) = (abs a + abs (a + b) + abs (b)) / 2

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
