module Y2017.Day01 where

import qualified Data.Text.IO as TIO
import qualified Data.Text    as T

import           Data.Char

part1 :: [Int] -> Int
part1 nums = sum $ zipWith diffval nums (tail nums)

part2 :: [Int] -> Int
part2 nums = sum $ zipWith diffval nums (drop (length nums `div` 2) (cycle nums))

diffval a b | a == b = a
            | a /= b = 0

main :: IO ()
main = do
  input <- TIO.readFile "src/Y2017/input01"
  let procs = map digitToInt $ filter isNumber $ T.unpack input
  tprint $ part1 procs
  tprint $ part2 procs

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
