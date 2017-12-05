module Day05 where

import           Data.Text             as T
import           Data.Text.IO          as TIO

import           Text.Megaparsec
import           Text.Megaparsec.Lexer as L
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
p = integer `sepBy` char '\n'

part1 :: [Integer] -> Integer
part1 = undefined

part2 :: [Integer] -> Integer
part2 = undefined
