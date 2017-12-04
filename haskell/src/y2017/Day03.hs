module Day03 where

import qualified Data.Text.IO as TIO
import qualified Data.Text    as T

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show

main = do
  tprint $ part1 289326

part1 :: Int -> Int
part1 1 = 0
part1 p = shell + abs((p - (4*shell^2-2*shell+1)) `mod` shell)
  where
    shell = ceiling $ (sqrt (fromIntegral n) - 1) / 2
    n = fromIntegral p


