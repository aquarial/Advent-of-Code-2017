{-# LANGUAGE OverloadedStrings #-}
module Day06 where

import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text  (Parser)

import qualified Data.Map              as M
import qualified Data.Set              as S
import           Data.List

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show

main = do
  input <- TIO.readFile "src/y2017/input06"
  case parse p "input06" input of
    Left  err   -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right jumps -> do
      tprint $ part1 jumps
      tprint $ part2 jumps
  return ()

p :: Parser [Int]
p = int `sepBy` char '\t'
  where
    int = fromInteger <$> L.integer

part1 :: Num t => [Int] -> t
part1 xs = walkout1 S.empty xs 0


walkout1 :: Num t => S.Set [Int] -> [Int] -> t -> t
walkout1 s a acc = if (S.member next s) then acc+1 else walkout1 (S.insert next s) next (acc+1)
  where
    next = walk a

part2 :: Num t => [Int] -> t
part2 xs = walkout M.empty xs 0

f (Just a) = a
f (Nothing) = error "aaaah"

walkout s a acc = if (M.member next s) then f(M.lookup (next) s) - acc else walkout (M.insert next acc s) next (acc+1)
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
