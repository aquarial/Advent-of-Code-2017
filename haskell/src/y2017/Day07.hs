{-# LANGUAGE OverloadedStrings #-}
module Day07 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text  (Parser)

import           Data.List
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show

main = do
  input <- TIO.readFile "src/y2017/input07"
  case parse p "input07" input of
    Left  err   -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right betterInput -> do
      tprint $ part1 betterInput
      tprint $ part2 betterInput
  return ()

p :: Parser [(String, Int, [String])]
p = program `sepBy` char '\n'

program :: Parser (String, Int, [String])
program = do
  name <- some letterChar
  string " ("
  size <- int
  string ")"
  deps <- option [] supports
  pure (name, size, deps)

int :: Parser Int
int = do
      change <- option id (negate <$ char '-')
      fromInteger . change <$> L.integer

supports :: Parser [String]
supports = string " -> "   *>   some letterChar `sepBy` string ", "


name (a, _, _) = a
size (_, a, _) = a
sups (_, _, a) = a


part1 :: [(String, Int, [String])] -> String
part1 xs = name $ head $ filter (appearsInNoSupports xs) xs

appearsInNoSupports :: [(String, Int, [String])] -> (String, Int, [String]) -> Bool
appearsInNoSupports xs x = not . any (elem (name x) . sups) $ xs



part2 xs = findImbalance xs root 0
  where
    root = head $ filter (appearsInNoSupports xs) xs

findImbalance :: [(String, Int, [String])] -> (String, Int, [String]) -> Int -> Int
findImbalance others curr actual = case findUniqueWeight supports supWeights of
                              Nothing  -> actual - sum supWeights
                              Just u   -> findImbalance others u (ordinary supWeights)
  where
    supporters x = filter (\o -> elem (name o) (sups x)) others
    supports = supporters curr
    supWeights = map weight supports
    weight x = size x + sum (map weight (supporters x))

ordinary :: [Int] -> Int
ordinary (x:xs) = if elem x xs then x else ordinary xs

findUniqueWeight :: [(String, Int, [String])] -> [Int] -> Maybe (String, Int, [String])
findUniqueWeight programs weights = case filter (snd) $ zip programs $ map (/=expected) weights of
                                      (prog,_):[] -> Just prog
                                      _           -> Nothing
  where
    expected = ordinary weights



