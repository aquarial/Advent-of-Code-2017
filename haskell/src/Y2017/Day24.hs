{-# LANGUAGE OverloadedStrings #-}
module Y2017.Day24 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text  (Parser)

import           Data.List


part1 = maximum . map strength . allPaths 0

part2 cs = maximum $ map strength $ filter (\l -> length l == mlen) paths
  where
    paths = allPaths 0 cs
    mlen = maximum (map length paths)

strength :: [Comp] -> Int
strength = sum . map compSum
  where
    compSum (Comp a b) = a + b

allPaths :: Int -> [Comp] -> [[Comp]]
allPaths c [] = []
allPaths c cs = case filter (fits c) cs of
                  [] -> [[]]
                  ccs -> concatMap (\x -> map (x:) (allPaths (other c x) (delete x cs))) ccs

other :: Int -> Comp -> Int
other c (Comp a b) = if c == a then b else a

fits :: Int -> Comp -> Bool
fits c (Comp a b) = c == a || c == b

data Comp = Comp Int Int deriving (Show, Eq)

p :: Parser [Comp]
p = parsecomp `sepEndBy` char '\n' <* eof

parsecomp = Comp <$> int <* char '/' <*> int

int :: Parser Int
int = do change <- option id (negate <$ char '-')
         fromInteger . change <$> L.integer

main :: IO ()
main = do
  input <- TIO.readFile "src/Y2017/input24"
  case parse p "input24" input of
    Left err -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right bi -> do
      tprint $ part1 bi
      tprint $ part2 bi

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
