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
import qualified Data.HashSet          as S
import qualified Data.Graph            as G

parta :: Text -> Int
parta = walk (0,0) S.empty 0 . gridarray

walk :: (Int, Int) -> S.HashSet (Int, Int) -> Int -> A.Array (Int, Int) Char -> Int
walk (_,128) _  acc  _   = acc
walk (128,y) seen acc grid = walk (0,y+1) seen acc grid
walk (x  ,y) seen acc grid | S.member (x,y) seen = walk (x+1,y) seen acc grid
                           | grid A.! (x,y) == '0' = walk (x+1,y) seen acc grid
                           | otherwise           = walk (x+1,y) (search seen [(x,y)] grid) (acc+1) grid

{-
search S.empty [(4,2)] $ gridarray "flqrgnkx"
sequence_ $ map putStrLn $ map (take 10) $ take 10 $ gridlist "flqrgnkx"
-}
search :: S.HashSet (Int,Int) -> [(Int,Int)] -> A.Array (Int,Int) Char -> S.HashSet (Int,Int)
search s []     _    = s
search s (p:ps) grid = search (S.insert p s) (filteredneigh s grid p ++ ps) grid

neighs (x,y) = filter inrange [(x+dx,y+dy) | dx <- [-1..1], dy <- [-1..1], dx*dy==0]
filteredneigh s grid p = filter (\i -> grid A.!i == '1' && not (S.member i s)) $ neighs p
--sameneigh p = filter (\i -> grid A.! i == grid A.! p) $ filter inrange $ neigh p
inrange = A.inRange ((0,0),(127,127))
--neigh (x,y) = filter (not . flip S.member ss) [(x+dx,y+dy) | dx <- [-1..1], dy <- [-1..1]]

gridarray :: Text -> A.Array (Int, Int) Char
gridarray = A.listArray ((0,0), (127,127)) . foldl (++) [] . gridlist

gridlist :: Text -> [[Char]]
gridlist x = map (free . hash) [ x <> "-" <> t | t <- map (T.pack . show) [0..127]]

free :: ByteString -> [Char]
free =  concatMap tobin . map fromhex . C8.unpack

fromhex x = fst $ head $ readHex [x]
tobin x = normalize $ showIntAtBase 2 intToDigit x ""
normalize i = replicate (4 - length i) '0' ++ i

--map popCount $ mconcat

main :: IO ()
main = do tprint $ parta "ffayrhll"

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
