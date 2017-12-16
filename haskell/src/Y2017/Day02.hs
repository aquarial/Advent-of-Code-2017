module Y2017.Day02 where

import qualified Data.Text.IO as TIO
import qualified Data.Text    as T
import           Data.Text    (Text)

import           Data.Char
import           Data.List
import           Data.Maybe (catMaybes)

import           Text.Megaparsec
import           Text.Megaparsec.Text (Parser)
import qualified Text.Megaparsec.Lexer as L


spreadsheet :: Parser [[Integer]]
spreadsheet = row `sepBy` char '\n'

row :: Parser [Integer]
row = L.integer `sepBy` tab

main :: IO ()
main = do
  input <- TIO.readFile "src/y2017/input02"
  let sheet = filter (not . null) $ maybe [] id $ parseMaybe spreadsheet input
      p1 = part1 sheet
      p2 = part2 sheet
  TIO.putStrLn $ T.pack $ show p1
  TIO.putStrLn $ T.pack $ show p2
  return ()


part1 :: [[Integer]] -> Integer
part1 nums = sum $ zipWith (-) (map maximum nums) (map minimum nums)


part2 :: [[Integer]] -> Integer
part2 nums = sum $ map (evenlyDivide . sort) nums
  where
    evenlyDivide (x:xs) = case filter (\a -> a `mod` x == 0) xs of
                            []      -> evenlyDivide xs
                            (ans:_) -> ans `div` x