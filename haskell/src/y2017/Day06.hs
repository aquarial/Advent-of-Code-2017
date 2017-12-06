{-# LANGUAGE OverloadedStrings #-}
module Day06 where

import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Text.Megaparsec.Text  (Parser)
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L


import qualified Data.Set  as S

import           Data.Maybe            (catMaybes)
import Debug.Trace

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show

main = do
  input <- TIO.readFile "src/y2017/input06"
  case parse p "input05" input of
    Left  err   -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right jumps -> do
      tprint $ part2 jumps
      --tprint $ part2 jumps
  return ()

p :: Parser [Int]
p = signedInt `sepBy` char '\t'
  where
    signedInt :: Parser Int
    signedInt = do
      neg <- negate <$ char '-' <|> pure id
      neg . fromInteger <$> L.integer

part1 a = a

part2 :: Num t => [Int] -> t
part2 xs = walkout S.empty xs 0


walkout :: Num t => S.Set [Int] -> [Int] -> t -> t
walkout s a acc = if (S.member next s) then acc+1 else walkout (S.insert next s) next (acc+1)
  where
    next = walk a

replace x [] _ = []
replace 0 (x:xs) i = i:xs
replace n (x:xs) i = x:replace (n-1) xs i

walk :: [Int] -> [Int]
walk ls = add1 ix eve
  where
  add1 [] e = e
  add1 (x:xs) e = add1 xs $ replace x e ((e !! x) + 1)
  eve :: [Int]
  eve = replace i ls 0
  i = findmax ls
  i2 = findmax eve
  ix :: [Int]
  ix = take (ls !! i) $map (\x -> x `mod` (length ls)) $ [i+1..]

--ls = [0, 2, 7, 0]



findmax :: [Int] -> Int
findmax as = fst . head . filter (\x -> (m == snd x)) $ zip [0..] as
  where
    m = maximum as
