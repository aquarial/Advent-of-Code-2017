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

import           Debug.Trace

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show

main = do
  input <- TIO.readFile "src/y2017/input07"
  case parse p "input07" input of
    Left  err   -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right betterInput -> do
      tprint $ part1 betterInput
      tprint $ part betterInput
  return ()

p :: Parser [(String, Int, [String])]
p = fileName `sepBy` char '\n'

fileName :: Parser (String, Int, [String])
fileName = do
  name <- some $ noneOf [' ']
  char ' '
  char '('
  size <- int
  char ')'
  deps <- supports <|> pure []
  return (name, size, deps)

supports :: Parser [String]
supports = do
      string (" -> " :: String)
      (sepBy word (string ", "))
  where
    word :: Parser String
    word =  some $ noneOf (", \n" :: String)


part1 xs = filter (appearsInSupports xs) xs
part xs = map (weight xs) xs

int = do
      change <- negate <$ char '-' <|> pure id
      fromInteger . change <$> L.integer

weight :: [(String, Int, [String])] -> (String, Int, [String]) -> (Int, String)
weight xs (name,w23,supports) = case length $ nub $ map fst ws of
                                     1 -> (w23 + sum (map fst ws),  concat $map snd $ filter (\i -> not (null (snd i))) ws)
                                     0 -> (w23 + sum (map fst ws),  concat $map snd $ filter (\i -> not (null (snd i))) ws)
                                     2 -> if (all null (map snd ws) )
                                          then trace (name ++ show (zip ws333 (map fst ws))) $ (0, show $map fst ws)
                                          else head $ filter (\i -> not (null (snd i))) ws
  where
    ws :: [(Int, String)]
    ws = map (weight xs) $ filter (\(n,_,_) -> elem n supports) xs
    ws333 = map (\(n,_,_) -> n) $ filter (\(n,_,_) -> elem n supports) xs

appearsInSupports xs (name,_,_) = all (\(_,_,c) -> notElem name c) xs

partstr xs = xs

replace :: Int -> a -> [a] -> [a]
replace i e []     = []
replace 0 e (x:xs) = e:xs
replace i e (x:xs) = x:replace (i-1) e xs
