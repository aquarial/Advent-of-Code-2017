{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Y2017.Day17 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Data.List
import qualified Data.Sequence         as Seq

parta d = take 10 $ walk d 1 (Seq.fromList [0])

--walk :: Int -> Int -> Seq.Seq Int -> [Maybe Int]
walk d v s = let d2 = (d+1) `mod` (Seq.length s)
             in (s,d2) : walk d (v+1) (v Seq.<| Seq.drop d2 s  Seq.>< (Seq.take d2 s) )

partb :: Int -> Int
partb d = millionthInsert 0 0 $ genAfter0Pairs (1,1) 0 d

millionthInsert :: Int -> Int -> [(Int, Int)] -> Int
millionthInsert !inserts !lastVal ((!valAfter0,!untilReplaced):xs) =
  if (inserts+untilReplaced > 50*10^6)
  then lastVal
  else millionthInsert (inserts+untilReplaced) (valAfter0) xs

genAfter0Pairs ::  (Int, Int) -> Int -> Int -> [(Int, Int)]
genAfter0Pairs (!count, !iters) !pos !distance =
  if pos == 0
  then (iters,count) : genAfter0Pairs (      1,iters+1) nextpos distance
  else                 genAfter0Pairs (count+1,iters+1) nextpos distance

  where
    nextpos = ((1+pos+distance)`mod` (iters+1))

main :: IO ()
main = do
  tprint $ parta 3
  tprint $ partb 394

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
