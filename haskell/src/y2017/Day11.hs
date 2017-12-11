{-# LANGUAGE OverloadedStrings #-}
module Day11 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text  (Parser)

data Move = S | SW | NW | N | NE | SE

p :: Parser [Move]
p = (move <$> word)`sepBy` char ','


move :: Text -> Move
move "se" = SE
move "s" = S
move "sw" = SW
move "ne" = NE
move "nw" = NW
move "n" = N

word :: Parser Text
word = T.pack <$> some letterChar

part1 = dist . last . walk (0,0)

part2 = maximum . map dist . walk (0,0)

dist (a,b) = (abs a + abs (a + b) + abs (b)) / 2

walk (a,b) (S :xs) = (a,b):walk (a,  b+1) xs
walk (a,b) (SE:xs) = (a,b):walk (a+1,b)   xs
walk (a,b) (SW:xs) = (a,b):walk (a-1,b+1) xs
walk (a,b) (N :xs) = (a,b):walk (a  ,b-1) xs
walk (a,b) (NE:xs) = (a,b):walk (a+1,b-1) xs
walk (a,b) (NW:xs) = (a,b):walk (a-1,b)   xs
walk  z      []      = [z]


main :: IO ()
main = do
  input <- TIO.readFile "src/y2017/input11"
  case parse p "input11" input of
    Left  err   -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right betterInput -> do
      tprint $ part1 betterInput
      tprint $ part2 betterInput
  return ()


tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
