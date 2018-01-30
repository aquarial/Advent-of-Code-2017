{-# LANGUAGE OverloadedStrings #-}
module Y2017.Day14 where

import           Y2017.Day10           (hash)

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as C8

import           Numeric
import           Data.Char
import           Data.Bits
import           Data.List
import           Data.Monoid

import qualified Data.Array            as A
import qualified Data.Graph            as G


parta :: Text -> Int
parta = length . filter (=='1') . concat . gridlist

partb :: Text -> Int
partb = length . G.stronglyConnComp . buildgraph . gridlist

type Coord = (Int,Int)

buildgraph :: [[Char]] -> [(Char, Coord, [Coord])]
buildgraph cs = [(cs!!x!!y, (x,y), filter isone (neighs (x,y))) | x <- [0..length cs - 1]
                                                                , y <- [0..length cs - 1]
                                                                , isone (x,y)]
  where isone (x,y) = cs!!x!!y == '1'

neighs (x,y) = filter inrange [(x+dx,y+dy) | dx <- [-1..1], dy <- [-1..1], dx*dy==0, dx /= dy]
  where inrange = A.inRange ((0,0),(127,127))

gridlist :: Text -> [[Char]]
gridlist x = map (tobinarystring . hash) [ x <> "-" <> t | t <- map (T.pack . show) [0..127]]

tobinarystring :: ByteString -> [Char]
tobinarystring =  concatMap tobin . map fromhex . C8.unpack
  where
    fromhex x = fst $ head $ readHex [x]
    tobin x = normalize $ showIntAtBase 2 intToDigit x ""
    normalize i = replicate (4 - length i) '0' ++ i

main :: IO ()
main = do tprint $ parta "ffayrhll"
          tprint $ partb "ffayrhll"

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
