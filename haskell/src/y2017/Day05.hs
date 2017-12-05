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
part1 = walk [] . map fromInteger
  where
    walk _  []     = 0
    walk as (b:bs) | b <= 0 = 1 + walk ( drop (-b) as )  ( reverse (take (-b) as) ++ [b+1] ++ bs )
    walk as (b:bs) | b >  0 = 1 + walk ( reverse (take b ((b+1):bs)) ++ as )  (drop b (b:bs))

part2 :: [Integer] -> Integer
part2 = undefined
