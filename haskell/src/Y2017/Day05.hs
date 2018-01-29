{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Y2017.Day05 where

import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.IO                as TIO

import           Data.Vector.Unboxed         (Vector)
import qualified Data.Vector.Unboxed         as V
import           Data.Vector.Unboxed.Mutable (MVector, STVector)
import qualified Data.Vector.Unboxed.Mutable as MV

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer  as L

import           Control.Monad
import qualified Control.Monad.ST            as ST

part1 :: Vector Int -> Int
part1 vec = ST.runST $ do v <- V.thaw vec
                          mwalk (+1) v 0 0

part2 :: Vector Int -> Int
part2 vec = ST.runST $ do v0 <- V.thaw vec
                          mwalk chang v0 0 0
  where
    chang x = if x < 3 then x+1 else x-1

mwalk :: (Int -> Int) -> MVector s Int -> Int -> Int -> ST.ST s Int
mwalk dx v !i !acc = if MV.length v <= i
                   then pure acc
                   else do x <- MV.read v i
                           MV.write v i (dx x)
                           mwalk dx v (i+x) (acc+1)

type Parser = Parsec Void Text

p :: Parser (Vector Int)
p = V.fromList <$> signedInt `sepBy` char '\n'
  where
    signedInt :: Parser Int
    signedInt = do
      neg <- negate <$ char '-' <|> pure id
      neg . fromInteger <$> L.decimal

main = do
  input <- TIO.readFile "src/Y2017/input05"
  case parse p "input05" input of
    Left  err   -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right jumps -> do
      tprint $ part1 jumps
      tprint $ part2 jumps


tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show

