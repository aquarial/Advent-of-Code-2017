module Day03 where

import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO

import           Data.Maybe          (catMaybes)
import           Data.List           (scanl')
import qualified Data.HashMap.Strict as HMap

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show

main = do
  tprint $ part1 289326
  tprint $ part2 289326

part1 :: Int -> Int
part1 1 = 0
part1 p = layer + abs((p - (4*layer^2 - 2*layer + 1)) `mod` layer)
  where
    layer = ceiling $ (sqrt n - 1) / 2
    n = fromIntegral p




part2 :: Int -> Int
part2 n = firstGood $ map (uncurry HMap.lookup) $ zip spiral $ drop 1 $ scanl' updateMap initMap spiral
  where
    initMap = HMap.fromList [((0,0), 1)]

    firstGood (Nothing:xs) = firstGood xs
    firstGood (Just n2:xs) = if n2 > n then n2 else firstGood xs

addSquare :: HMap.HashMap (Int, Int) Int -> (Int, Int) -> HMap.HashMap (Int, Int) Int
addSquare pmap (x0,y0) = HMap.insert (x0,y0) value pmap
  where
    value = sum $ map (\k -> HMap.lookupDefault 0 k oldmap) [(x0+x,y0+y) | x <- [-1..1], y <- [-1..1]]

spiral :: [(Int, Int)]
spiral = walk [(1,0)] 1
  where
    walk (x:[]) n = x : walk (layer n x) (n+1)
    walk (x:xs) n = x : walk xs n

layer :: Int -> (Int, Int) -> [(Int, Int)]
layer n prev = r & d & l & u prev
  where
    u (x,y) = [(x,y+t) | t <- [1..2*n-1]]
    l (x,y) = [(x-t,y) | t <- [1..2*n  ]]
    d (x,y) = [(x,y-t) | t <- [1..2*n  ]]
    r (x,y) = [(x+t,y) | t <- [1..2*n+1]]

    infixr 0 &
    (&) :: (t -> [t]) -> [t] -> [t]
    (&) dir out = out ++ dir (last out)
