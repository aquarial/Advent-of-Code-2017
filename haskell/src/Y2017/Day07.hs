{-# LANGUAGE OverloadedStrings #-}
module Y2017.Day07 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Data.List
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show

main = do
  input <- TIO.readFile "src/Y2017/input07"
  case parse p "input07" input of
    Left  err   -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right betterInput -> do
      tprint $ part1 betterInput
      tprint $ part2 betterInput
  return ()

type Parser = Parsec Void Text

p :: Parser [(Text, Int, [Text])]
p = program `sepBy` char '\n'

program :: Parser (Text, Int, [Text])
program = do
  name <- some letterChar
  string " ("
  size <- int
  string ")"
  deps <- option [] supports
  pure (T.pack name, size, deps)

int :: Parser Int
int = do
      change <- option id (negate <$ char '-')
      fromInteger . change <$> L.decimal

supports :: Parser [Text]
supports = string " -> "   *>  (T.pack <$> some letterChar) `sepBy` string ", "


name (a, _, _) = a
size (_, a, _) = a
sups (_, _, a) = a


part1 :: [(Text, Int, [Text])] -> Text
part1 xs = name $ head $ filter (appearsInNoSupports xs) xs

appearsInNoSupports :: [(Text, Int, [Text])] -> (Text, Int, [Text]) -> Bool
appearsInNoSupports xs x = not . any (elem (name x) . sups) $ xs




part2 :: [(Text, Int, [Text])] -> Int
part2 xs = findImbalance xs root 0
  where
    root = head $ filter (appearsInNoSupports xs) xs

findImbalance :: [(Text, Int, [Text])] -> (Text, Int, [Text]) -> Int -> Int
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

findUniqueWeight :: [(Text, Int, [Text])] -> [Int] -> Maybe (Text, Int, [Text])
findUniqueWeight programs weights = case filter (snd) $ zip programs $ map (/=expected) weights of
                                      (prog,_):[] -> Just prog
                                      _           -> Nothing
  where
    expected = ordinary weights
