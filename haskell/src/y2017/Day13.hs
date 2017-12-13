{-# LANGUAGE OverloadedStrings #-}
module Day13 where

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

data Wall = Wall { _depth :: Int
                 , _range :: Int } deriving Show

p :: Parser [Wall]
p = line `sepEndBy` char '\n'

line :: Parser Wall
line = do d <- int
          string ": "
          r <- int
          pure $ Wall d r

int :: Parser Int
int = do change <- option id (negate <$ char '-')
         fromInteger . change <$> L.integer

part1 xs = findIndex null $ map (\o -> walk2 o xs) [0..]

walk2 :: Int -> [Wall] -> [Wall]
walk2 offset xs = filter willBeZero xs
  where
    willBeZero (Wall d r) = (d+offset) `mod` (2*(r-1)) == 0

main :: IO ()
main = do
  input <- TIO.readFile "src/y2017/input13"
  case parse p "input13" input of
    Left err -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right bi -> do
      --tprint $ walk2 0 bi
      tprint $ part1 bi

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
