module Y2017.Day03 where

import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO

import           Data.Maybe          (catMaybes)
import           Data.List           (scanl')
import qualified Data.HashMap.Strict as HMap

main = do
  tprint $ part1 289326
  tprint $ part2 289326

part1 :: Int -> Int
part1 1 = 0
part1 n = layer + abs( ((n - 1) `mod` (2*layer)) - layer )
  where
    layer = ceiling $ 0.5 * (sqrt floatn - 1)
    floatn = fromIntegral n


part2 :: Int -> Int
part2 n = firstGood $ map (uncurry HMap.lookup) $ zip spiral $ drop 1 $ scanl' updateMap initMap spiral
  where
    initMap = HMap.fromList [((0,0), 1)]

    firstGood (Nothing:xs) = firstGood xs
    firstGood (Just n2:xs) = if n2 > n then n2 else firstGood xs

    -- uncurry :: (a   -> b   -> c)          -> (a, b) -> c
    -- lookup  ::  key -> map -> Maybe Value

updateMap :: HMap.HashMap (Int, Int) Int -> (Int, Int) -> HMap.HashMap (Int, Int) Int
updateMap oldmap (x0,y0) = HMap.insert (x0,y0) value oldmap
  where
    value = sum $ map (\k -> HMap.lookupDefault 0 k oldmap) [(x0+x,y0+y) | x <- [-1..1], y <- [-1..1]]


spiral :: [(Int, Int)]
spiral = walk (0,0) (cycle [r,u,l,d]) layers
  where
    r (x,y) = (x+1,y)
    u (x,y) = (x,y+1)
    l (x,y) = (x-1,y)
    d (x,y) = (x,y-1)

walk :: t -> [t -> t] -> [Int] -> [t]
walk p (m:moves) (0:steps) =     walk    p     moves       steps
walk p (m:moves) (s:steps) = p : walk (m p) (m:moves) (s-1:steps)

layers :: [Int]
layers = concat [ [x,x] | x <- [1..]]

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
