module Day03 where

import qualified Data.Text.IO as TIO
import qualified Data.Text    as T

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show

main = do
  tprint $ part1 289326

part1 :: Int -> Int
part1 1 = 0
part1 p = layer + abs((p - (4*layer^2 - 2*layer + 1)) `mod` layer)
  where
    layer = ceiling $ (sqrt n - 1) / 2
    n = fromIntegral p


