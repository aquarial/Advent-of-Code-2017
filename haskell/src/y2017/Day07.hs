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
import qualified Data.Maybe            (maybeToList)

import           Debug.Trace

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
part1 xs = head $ map name $ filter (appearsInNoSupports xs) xs

appearsInNoSupports :: [(String, Int, [String])] -> (String, Int, [String]) -> Bool
appearsInNoSupports xs x = not . any (elem (name x) . sups) $ xs



part2 xs = map (weight xs) xs


weight :: [(String, Int, [String])] -> (String, Int, [String]) -> (Int, String)
weight xs (name,w23,supports) = case length $ nub $ map fst ws of
                                     2 -> if (all null (map snd ws) )
                                          then trace (name ++ show (zip ws333 (map fst ws))) $ (0, show $map fst ws)
                                          else head $ filter (\i -> not (null (snd i))) ws
                                     _ -> (w23 + sum (map fst ws),  concat $map snd $ filter (\i -> not (null (snd i))) ws)
  where
    ws :: [(Int, String)]
    ws = map (weight xs) $ filter (\(n,_,_) -> elem n supports) xs
    ws333 = map (\(n,_,_) -> n) $ filter (\(n,_,_) -> elem n supports) xs


