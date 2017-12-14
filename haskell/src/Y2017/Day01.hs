module Y2017.Day01 where

import qualified Data.Text.IO as TIO
import qualified Data.Text    as T

import           Data.Char



main :: IO ()
main = do
  input <- TIO.readFile "src/y2017/input01"
  let processed = map digitToInt $ filter isNumber $ T.unpack input
      p1 = part1 processed
      p2 = part2 processed
  TIO.putStrLn $ T.pack $ show p1
  TIO.putStrLn $ T.pack $ show p2
  return ()

part1 :: [Int] -> Int
part1 nums = walk (head nums) nums
  where
    walk :: Int -> [Int] -> Int
    walk f []     = error "Empty list"
    walk f (a:[]) = if a == f then f else 0
    walk f (a:as) = if a == head as then a + walk f as
                                    else     walk f as


part2 :: [Int] -> Int
part2 nums = sum $ zipWith (\a b -> if a == b then a else 0) nums (drop l nums ++ take l nums)
  where
    l = length nums `div` 2
