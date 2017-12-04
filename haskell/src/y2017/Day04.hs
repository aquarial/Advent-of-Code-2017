{-# LANGUAGE OverloadedStrings #-}
module Day04 where

import qualified Data.Text.IO as TIO
import qualified Data.Text    as T
import           Data.Text    (Text)

import           Data.Char
import           Data.List
import           Data.Maybe (catMaybes)

import           Text.Megaparsec
import           Text.Megaparsec.Text (Parser)
import qualified Text.Megaparsec.Lexer as L


row :: Parser [Integer]
row = sepBy L.integer space

main :: IO ()
main = do
  input <- TIO.readFile "src/y2017/input04"
  let processed =  map (T.splitOn " ") $ T.lines input
      p1 = part1 processed
      p2 = length $ filter part2 processed
  TIO.putStrLn $ T.pack $ show p1
  return ()


part1 a = length a == length (nub a)


part2 [] = True
part2 (a:as) = not (any (\b -> sort (T.unpack a) == sort(T.unpack b)) as) && part2 as
