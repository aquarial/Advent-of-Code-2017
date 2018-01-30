{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Y2017.Day17 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Data.Maybe            (catMaybes)
import           Data.List
import qualified Data.Sequence         as Seq

parta :: Int -> Int
parta d = head $ catMaybes $ map after2017 $ drop 2017 $ walk d 1 0 (Seq.fromList [0])

after2017 :: (Num a, Eq a) => Seq.Seq a -> Maybe a
after2017 s = case Seq.findIndexL (==2017) s of
                Nothing -> Nothing
                Just i -> s Seq.!? (i+1)

walk :: Num a => Int -> a -> Int -> Seq.Seq a -> [Seq.Seq a]
walk d v i s = s : walk d (v+1) i2 (Seq.insertAt (i+1) v s)
  where
    i2 = (i+d+1) `mod` (Seq.length s + 1)


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
  tprint $ parta 394
  tprint $ partb 394

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
