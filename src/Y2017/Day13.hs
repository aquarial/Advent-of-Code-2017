{-# LANGUAGE OverloadedStrings #-}
module Y2017.Day13 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Data.List

data Wall = Wall { _depth :: Int
                 , _range :: Int } deriving Show

part1 :: [Wall] -> Int
part1 ws = sum $ map (\(Wall d r) -> d*r) $ findCatches 0 ws

part2 :: [Wall] -> Maybe Int
part2 ws = findIndex null $ [findCatches o ws | o <- [0..]]

findCatches :: Int -> [Wall] -> [Wall]
findCatches offset = filter willBeZero
  where
    willBeZero (Wall d r) = (d+offset) `rem` (2*(r-1)) == 0

type Parser = Parsec Void Text

p :: Parser [Wall]
p = line `sepEndBy` char '\n'

line :: Parser Wall
line = do d <- int
          string ": "
          r <- int
          pure $ Wall d r

int :: Parser Int
int = do change <- option id (negate <$ char '-')
         fromInteger . change <$> L.decimal


main :: IO ()
main = do
  input <- TIO.readFile "src/Y2017/input13"
  case parse p "input13" input of
    Left err -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right bi -> do
      tprint $ part1 bi
      tprint $ part2 bi

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
