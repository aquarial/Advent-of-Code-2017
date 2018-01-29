{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Y2017.Day17 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Data.List

partb :: Int -> Int
partb d = get 0 0 $ generator (1,1) 0 d

get :: Int -> Int -> [(Int, Int)] -> Int
get !inserts !lastVal ((!valAfter0,!untilReplaced):xs) =
  if (inserts+untilReplaced > 50*10^6)
  then lastVal
  else get (inserts+untilReplaced) (valAfter0) xs

generator ::  (Int, Int) -> Int -> Int -> [(Int, Int)]
generator (!count, !iters) !pos !distance =
  if pos == 0
  then (iters,count) : generator (      1,iters+1) nextpos distance
  else                 generator (count+1,iters+1) nextpos distance

  where
    nextpos = ((1+pos+distance)`mod` (iters+1))

main :: IO ()
main = do
  tprint $ partb 394

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
