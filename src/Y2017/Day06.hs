{-# LANGUAGE OverloadedStrings #-}
module Y2017.Day06 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


import qualified Data.Map              as M
import qualified Data.Set              as S
import           Data.List

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show

main = do
  input <- TIO.readFile "src/Y2017/input06"
  case parse p "input06" input of
    Left  err   -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right jumps -> do
      let (a,b) = solve jumps
      tprint a
      tprint b
  return ()

type Parser = Parsec Void Text

p :: Parser [Int]
p = int `sepBy` char '\t'
  where
    int = fromInteger <$> L.decimal


solve :: Num t => [Int] -> (t, t)
solve xs = walkout M.empty xs 0

walkout :: Num t => M.Map [Int] t -> [Int] -> t -> (t, t)
walkout seen blocks acc = case M.lookup next seen of
                       Nothing -> walkout (M.insert next acc seen) next (acc+1)
                       Just v  -> (acc, acc-v)
  where
    next = onecycle blocks

onecycle :: [Int] -> [Int]
onecycle ls = add1at (replace maxindex 0 ls) ixs
  where
  ixs = take (ls !! maxindex) $ map (\x -> x `mod` length ls) [maxindex+1..]
  maxindex = findmax ls

add1at :: Num a => [a] -> [Int] -> [a]
add1at = foldl' (\ls index -> replace index ((ls !! index) + 1) ls)

replace :: Int -> a -> [a] -> [a]
replace i e []     = []
replace 0 e (x:xs) = e:xs
replace i e (x:xs) = x:replace (i-1) e xs

findmax :: [Int] -> Int
findmax xs = case findIndex (\n -> n == maximum xs) xs of
               Nothing -> 0
               Just ix -> ix
