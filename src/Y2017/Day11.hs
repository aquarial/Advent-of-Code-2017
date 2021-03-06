{-# LANGUAGE OverloadedStrings #-}
module Y2017.Day11 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

data Move = S | SW | NW | N | NE | SE

type Parser = Parsec Void Text

p :: Parser [Move]
p = (move <$> word) `sepBy` char ','


move :: Text -> Move
move "se" = SE
move "s"  = S
move "sw" = SW
move "ne" = NE
move "nw" = NW
move "n"  = N

word :: Parser Text
word = T.pack <$> some letterChar

part1 :: [Move] -> Double
part1 = dist . foldl walk (0,0)

part2 :: [Move] -> Double
part2 = maximum . map dist . scanl walk (0,0)

dist (a,b) = (abs a + abs (a + b) + abs b) / 2

walk (a,b) S  = (a,  b+1)
walk (a,b) SE = (a+1,b)
walk (a,b) SW = (a-1,b+1)
walk (a,b) N  = (a  ,b-1)
walk (a,b) NE = (a+1,b-1)
walk (a,b) NW = (a-1,b)

main :: IO ()
main = do
  input <- TIO.readFile "src/Y2017/input11"
  case parse p "input11" input of
    Left  err   -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right betterInput -> do
      tprint $ part1 betterInput
      tprint $ part2 betterInput
  return ()


tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
