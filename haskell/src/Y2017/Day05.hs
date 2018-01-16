{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Y2017.Day05 where

import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Data.Vector.Unboxed           (Vector)
import           Data.Vector.Unboxed.Mutable   (MVector, STVector)
import qualified Data.Vector.Unboxed           as V
import qualified Data.Vector.Unboxed.Mutable   as MV

import           Text.Megaparsec.Text  (Parser)
import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L

import           Control.Monad
import qualified Control.Monad.ST      as ST


tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show

main = do
  input <- TIO.readFile "src/Y2017/input05"
  case parse p "input05" input of
    Left  err   -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right jumps -> do
      tprint $ part1_mut jumps
      tprint $ part2_mut jumps
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

part2_mut vec = ST.runST $ do v0 <- V.thaw vec
                              walk v0 0 0
  where
    chang x = if x < 3 then x+1 else x-1

    walk :: MVector s Int -> Int -> Int -> ST.ST s Int
    walk v !steps !i = if i < 0 || i >= MV.length v
                       then pure steps
                       else do x0 <- MV.read v i
                               MV.write v i $! chang x0
                               walk v (steps+1) (i+x0)
