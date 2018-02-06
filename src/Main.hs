module Main (main) where


import Criterion.Main

import qualified Y2017.Day01 as D01
import qualified Y2017.Day02 as D02
import qualified Y2017.Day03 as D03
import qualified Y2017.Day04 as D04
import qualified Y2017.Day05 as D05
import qualified Y2017.Day06 as D06
import qualified Y2017.Day07 as D07
import qualified Y2017.Day08 as D08
import qualified Y2017.Day09 as D09
import qualified Y2017.Day10 as D10
import qualified Y2017.Day11 as D11
import qualified Y2017.Day12 as D12
import qualified Y2017.Day13 as D13
import qualified Y2017.Day14 as D14
import qualified Y2017.Day15 as D15
import qualified Y2017.Day16 as D16
import qualified Y2017.Day17 as D17
import qualified Y2017.Day18 as D18
import qualified Y2017.Day19 as D19
import qualified Y2017.Day20 as D20
import qualified Y2017.Day21 as D21
import qualified Y2017.Day22 as D22
import qualified Y2017.Day23 as D23
import qualified Y2017.Day24 as D24
import qualified Y2017.Day25 as D25

main :: IO ()
main = benchmark

benchmark :: IO ()
benchmark = defaultMain [ bgroup "days" [ bench (day n) $ nfIO f | (n,f) <- zip [1..25] allmains] ]

day :: Int -> [Char]
day n | n <  10 = "0" ++ show n
      | n >= 10 = show n

allmains :: [IO ()]
allmains = [D01.main, D02.main, D03.main, D04.main, D05.main,
            D06.main, D07.main, D08.main, D09.main, D10.main,
            D11.main, D12.main, D13.main, D14.main, D15.main,
            D16.main, D17.main, D18.main, D19.main, D20.main,
            D21.main, D22.main, D23.main, D24.main, D25.main]
