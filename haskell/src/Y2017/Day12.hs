{-# LANGUAGE OverloadedStrings #-}
module Y2017.Day12 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.Graph            as G

type Parser = Parsec Void Text

p :: Parser (G.Graph)
p = buildgraph <$> line `sepEndBy` char '\n'

line :: Parser (Int, [Int])
line = (,) <$> (int <* string " <-> ") <*> (int `sepBy` string ", ")

int :: Parser Int
int = do change <- option id (negate <$ char '-')
         fromInteger . change <$> L.decimal

buildgraph :: [(Int, [Int])] -> G.Graph
buildgraph xs = G.buildG (0, fst $ last xs) alledges
  where
    alledges = concatMap mkedges xs
    mkedges (node,connected) = zip (repeat node) connected

part1 :: G.Graph -> Int
part1 g = length $ G.reachable g 0

part2 :: G.Graph -> Int
part2 g = length $ G.scc g
-- StronglyConnectedComponents

main :: IO ()
main = do
  input <- TIO.readFile "src/Y2017/input12"
  case parse p "input12" input of
    Left err -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right bi -> do
      tprint $ part1 bi
      tprint $ part2 bi

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
