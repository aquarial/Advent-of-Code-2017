{-# LANGUAGE OverloadedStrings #-}
module DayTHEDAY where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import qualified Data.Text.IO          as TIO

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text  (Parser)

import           Data.List

import qualified Data.Map.Strict       as M
import qualified Data.Set              as S


p :: Parser [Int]
p = (int `sepBy` char ',')

int :: Parser Int
int = do
      change <- option id (negate <$ char '-')
      fromInteger . change <$> L.integer

part1 :: [Int] -> Integer
part1 = product . take 2 . oneround

--part2 :: [Int] -> Text
part2 xs = 3

oneround xs = let (result, startPos) = walk 0 xs [0..255]
              in cycleL startPos result

walk :: Int -> [Int] -> [a] -> ([a], Int)
walk skip []         elems = (elems, 0)
walk skip (i:inputs) elems = shiftedStart <$> (walk (skip+1) inputs $ cycleL (i+skip) reved)
  where
    shiftedStart start = (start+skip+i) `mod` length elems
    reved = reverse (take i elems) ++ drop i elems

cycleL :: Int -> [a] -> [a]
cycleL i xs = let il = i `mod` length xs
              in drop il xs ++ take il xs

{-
0 1 2 3 4
3 4 2 1 0
1 2 4 3 0
3 0 1 2 4
0 3 4 2 1

-}

main :: IO ()
main = do
  let input = "63,144,180,149,1,255,167,84,125,65,188,0,2,254,229,24"
  case parse p "inputTHEDAY" input of
    Left  err   -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right betterInput -> do
      tprint $ part1 betterInput

  tprint $ part2 $ map fromIntegral $ B.unpack $ TE.encodeUtf8 input

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
