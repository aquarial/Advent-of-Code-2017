{-# LANGUAGE OverloadedStrings #-}
module Y2017.Day17 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text  (Parser)

import           Data.List
import qualified Data.Map.Strict       as M
import qualified Data.HashSet          as S
import qualified Data.Graph            as G

parta d =  get 0 0 $ generator 1 0 d 1

get total val ((value,count):xs) = if (total+count > 50*10^6) then val else get (total+count) (value) xs

generator count position distance iters = if position == 0
                                          then (iters,count) : generator 1 nextpos distance (iters+1)
                                          else                 generator (count+1) nextpos distance (iters+1)
  where
    nextpos = ((1+position+distance)`mod` (iters+1))
{-
take 100 $ map (\x -> (head x, length x)) $  group $ parta 394
[(0,1),(1,2),(3,5),(8,15),(23,33),(56,109),(165,786),(951,959),(1910,125),(2035,682),(2717,  C-c C-cInterrupted.

c1 p0 d3 i2

0 (2) 1
c1 p1 d3 i3

0 2 (3) 1
c2 p1 d3 i4

0 2 (4) 3 1
c3 p0 d3 i5

0 (5) 2  4  3  1
c1 p3 d3 i6

Right (1,0,3,2)
Right (1,0,3,3)
Left (1,1,3,4)
Right (2,0,3,5)
Left (1,4,3,6)
Left (2,1,3,7),
Left (3,4,3,8),
Left (4,7,3,9),
Left (5,1,3,10),
Left (6,4,3,11)]
-}
main :: IO ()
main = do
  --tprint $ parta 394
  return ()

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
