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
    walk i = case (lr (i-1)+1 <= p && p <= ur i,   ur i <= p && p <= lr i) of
               (True, _) -> trace ("1"++show i) $ 2*i - abs(p - ur i)
               (_, True) -> trace ("2"++show i) $ 2*i - abs( (p - ur i) `mod` (ul i - ur i) - (u i - ur i) )
               (_,    _) -> trace ("3"++show i) $ walk (i+1)

ul n = 4*n^2       + 1
u  n = 4*n^2 - 1*n + 1
ur n = 4*n^2 - 2*n + 1
r  n = 4*n^2 - 3*n + 1
lr n = 4*n^2 + 4*n + 1
