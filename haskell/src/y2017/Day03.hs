module Day03 where

import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Debug.Trace

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show

main = do
  tprint $ part1 289326

part1 :: Int -> Int
part1 p = if p == 1 then 0 else walk 1
  where
    walk i = if lr (i-1) < p && p <= lr i
             then i + abs((p - ur i) `mod` i)
             else (walk (i+1))

ur n = 4*n^2 - 2*n + 1
lr n = 4*n^2 + 4*n + 1
