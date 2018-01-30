module Y2017.Day02 where

import qualified Data.Text.IO as TIO
import qualified Data.Text    as T
import           Data.Text    (Text)

import           Data.Void
import           Data.Char
import           Data.List
import           Data.Maybe (catMaybes)

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


part1 :: [[Integer]] -> Integer
part1 nums = sum $ zipWith (-) (map maximum nums) (map minimum nums)

part2 :: [[Integer]] -> Integer
part2 nums = sum $ map (evenlyDivide . sort) nums
  where
    evenlyDivide (x:xs) = case filter ((==0) . (`mod` x)) xs of
                            (big:_) -> big `div` x
                            []      -> evenlyDivide xs


type Parser = Parsec Void Text

spreadsheet :: Parser [[Integer]]
spreadsheet = row `sepEndBy` char '\n' <* eof

row :: Parser [Integer]
row = L.decimal `sepBy1` tab

main :: IO ()
main = do
  input <- TIO.readFile "src/Y2017/input02"
  case parse spreadsheet "Day02" input of
    Left err -> tprint $ parseErrorPretty err
    Right bi -> do
      tprint $ part1 bi
      tprint $ part2 bi

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
