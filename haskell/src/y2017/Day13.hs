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
                 , _range :: Int
                 , _state :: Int
                 , _dir :: Dir} deriving Show

data Dir = U | D deriving Show

p :: Parser [Wall]
p = line `sepEndBy` char '\n'

line :: Parser Wall
line = do d <- int
          string ": "
          r <- int
          pure $ Wall d r 0 D

int :: Parser Int
int = do change <- option id (negate <$ char '-')
         fromInteger . change <$> L.integer

part1 = walk $ -1

data TRI a b c = A a | B b | C c deriving (Show)

walk pos []    = 0
walk pos (w:walls) | _depth w > pos+1 =  walk (pos+1) (map step (w:walls))
                   | _state w == 0    = _depth w * _range w + walk (pos+1) (map step walls)
                   | otherwise        =  walk (pos+1) (map step walls)
--_depth w * _range w

step :: Wall -> Wall
step (Wall d r s U) = if s-1 <  0 then Wall d r (s+1) D else Wall d r (s-1) U
step (Wall d r s D) = if s+1 >= r then Wall d r (s-1) U else Wall d r (s+1) D

main :: IO ()
main = do
  input <- TIO.readFile "src/y2017/input13"
  case parse p "input13" input of
    Left err -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right bi -> do
      tprint $ part1 bi

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
