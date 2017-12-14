{-# LANGUAGE OverloadedStrings #-}
module Y2017.Day04 where

import qualified Data.Text.IO as TIO
import qualified Data.Text    as T

import           Data.List

main :: IO ()
main = do
  input <- TIO.readFile "src/y2017/input04"
  let processed = map (map T.unpack . T.splitOn " ") $ T.lines input
      p1 = length $ filter part1 processed
      p2 = length $ filter part2 processed
  TIO.putStrLn $ T.pack $ show p1
  TIO.putStrLn $ T.pack $ show p2
  return ()


part1 :: Eq a => [a] -> Bool
part1 it = length it == length (nub it)


part2 :: Ord a => [[a]] -> Bool
part2 [] = True
part2 (a:as) = all (\e -> sort e /= sort a) as && part2 as
