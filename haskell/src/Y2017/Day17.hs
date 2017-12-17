{-# LANGUAGE OverloadedStrings #-}
module Y2017.Day17 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Data.List

parta d = get 0 0 $ generator 1 0 d 1

get total val ((value,count):xs) = if (total+count > 50*10^6) then val else get (total+count) (value) xs

generator count position distance iters = if position == 0
                                          then (iters,count) : generator         1 nextpos distance (iters+1)
                                          else                 generator (count+1) nextpos distance (iters+1)
  where
    nextpos = ((1+position+distance)`mod` (iters+1))

main :: IO ()
main = do
  --tprint $ parta 394
  return ()

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
