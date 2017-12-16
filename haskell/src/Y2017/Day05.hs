{-# LANGUAGE OverloadedStrings #-}
module Y2017.Day05 where

import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Data.Vector           (Vector)
import           Data.Vector.Mutable   (MVector, STVector)
import qualified Data.Vector           as V
import qualified Data.Vector.Mutable   as MV

import           Text.Megaparsec.Text  (Parser)
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L

import           Control.Monad
import qualified Control.Monad.ST      as ST


tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show

main = do
  input <- TIO.readFile "src/y2017/input05"
  case parse p "input05" input of
    Left  err   -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right jumps -> do
      tprint $ part1_mut jumps
      tprint $ part2 jumps
  return ()

p :: Parser (Vector Int)
p = V.fromList <$> signedInt `sepBy` char '\n'
  where
    signedInt :: Parser Int
    signedInt = do
      neg <- negate <$ char '-' <|> pure id
      neg . fromInteger <$> L.integer


part1_mut :: Num a => Vector Int -> a
part1_mut vec = ST.runST $ do v <- V.thaw vec
                              mwalk v 0 0
  where
    mwalk v i acc = if MV.length v <= i
                    then pure acc
                    else do x <- MV.read v i
                            MV.write v i $! x+1
                            mwalk v (i+x) (acc+1)


part2 :: Vector Int -> Integer
part2 = walk change . V.toList
  where
    change x = if x < 3 then x+1 else x-1

walk :: Num a => (Int -> Int) -> [Int] -> a
walk changer list = walkacc 0 [] list
  where
    walkacc acc _  []     = acc
    walkacc acc as (b:bs) | b <= 0 = walkacc (acc+1) ( drop (-b) as )  ( reverse (take (-b) as) ++ [changer b] ++ bs )
    walkacc acc as (b:bs) | b >  0 = walkacc (acc+1) ( reverse (take b ((changer b):bs)) ++ as )  (drop b (b:bs))