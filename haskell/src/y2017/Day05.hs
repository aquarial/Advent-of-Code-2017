{-# LANGUAGE OverloadedStrings #-}
module Day05 where

import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text  (Parser)



tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show

main = do
  input <- TIO.readFile "src/y2017/input05"
  case parse p "input05" input of
    Left  err   -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right jumps -> do
      tprint $ part1 jumps
      tprint $ part2 jumps
  return ()

p :: Parser [Integer]
p = signedInt `sepBy` char '\n'
  where
    signedInt :: Parser Integer
    signedInt = do
      neg <- negate <$ char '-' <|> pure id
      neg <$> L.integer

part1 :: [Integer] -> Integer
part1 = walk (+1) . map fromInteger

part2 :: [Integer] -> Integer
part2 = walk change . map fromInteger
  where
    change x = if x < 3 then x+1 else x-1

walk :: Num a => (Int -> Int) -> [Int] -> a
walk changer list = walkacc 0 [] list
  where
    walkacc acc _  []     = acc
    walkacc acc as (b:bs) | b <= 0 = walkacc (acc+1) ( drop (-b) as )  ( reverse (take (-b) as) ++ [changer b] ++ bs )
    walkacc acc as (b:bs) | b >  0 = walkacc (acc+1) ( reverse (take b ((changer b):bs)) ++ as )  (drop b (b:bs))
