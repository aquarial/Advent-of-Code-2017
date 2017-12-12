{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Day12 where

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

type Comm = (Int, [Int])

p :: Parser (G.Graph)
p = buildgraph <$> (line `sepBy` char '\n')

buildgraph :: [Comm] -> G.Graph
buildgraph xs = G.buildG (0, fst $ last xs) (concatMap mkedges xs)
  where
    mkedges (node,connected) = zip (repeat node) connected

line :: Parser Comm
line = (,) <$> (int <* string " <-> ") <*> (int `sepBy` string ", ")

int :: Parser Int
int = do change <- option id (negate <$ char '-')
         fromInteger . change <$> L.integer

part1 :: G.Graph -> Int
part1 g = length $ G.reachable g 0

part2 :: G.Graph -> Int
part2 g = length $ G.scc g

main :: IO ()
main = do
  input <- TIO.readFile "src/y2017/input12"
  case parse p "input12" input of
    Left err -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right bi -> do
      tprint $ part1 bi
      tprint $ part2 bi

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
