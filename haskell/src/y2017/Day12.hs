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



type Comm = (Int, [Int])

p :: Parser [Comm]
p = line `sepBy` char '\n'

line = do
  name <- int
  string " <-> "
  targets <- int `sepBy` (string (", "::String))
  pure (name, targets)

int :: Parser Int
int = do
      change <- option id (negate <$ char '-')
      fromInteger . change <$> L.integer


partA = last . take 2000 . walk []
--partA = take 10 . walk []

chunks [] = []
chunks (x:xs) = x : chunks (drop 100 xs)

--walk :: [S.Set Int] -> [Comm] -> [Int]
walk !ss ((!n,!ts):xs) = if not (any (\s -> S.member n s) ss)
                         then length ss:walk ss2 (xs++[(n,ts)])
                         else  length ss:walk ((foldl1 S.union (filter (\s -> S.member n s) ss2))
                                           :
                                           (filter (\s -> not (S.member n s)) ss2)) (xs++[(n,ts)])
  where
    ss2 = S.fromList(n:(ts::[Int])) : ss




main :: IO ()
main = do
  input <- TIO.readFile "src/y2017/input12"
  case parse p "input12" input of
    Left  err   -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right betterInput -> do
      tprint $ partA betterInput
  return ()

test input = case parse p "test" input of
               Left  err -> error $ parseErrorPretty err
               Right bi  -> partA bi


tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
