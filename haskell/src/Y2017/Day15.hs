{-# LANGUAGE OverloadedStrings #-}
module Y2017.Day15 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text  (Parser)

import           Control.Monad
import           Data.Bits
import           Data.List
import qualified Data.Map.Strict       as M
import qualified Data.HashSet          as S
import qualified Data.Graph            as G

import Numeric
import Data.Char
tostr d = showIntAtBase 2 intToDigit d ""

generator starts mults name = do s <- M.lookup name starts
                                 m <- M.lookup name mults
                                 return $ tail $ iterate (\x -> x*m `mod` 2147483647) s

test = do as <- generator starts mults 'A'
          bs <- generator starts mults 'B'
          pure $ (,) (judgeCount 5 [as,bs]) $ length $ filter (==0) $ take 5 $ zipWith bitcount as bs

parta starts = case sequence (map (generator starts mults) ['A','B']) of
                 Nothing -> Nothing
                 Just gs -> Just $ judgeCount (40*10^6) gs

mults  = M.fromList [('A',16807), ('B', 48271)]
starts = M.fromList [('A',65)   , ('B', 8921) ]

judgeCount :: Int -> [[Integer]] -> Int
judgeCount num (a:b:_) = length $ filter (==0) $ take num $ zipWith bitcount a b

bitcount :: Integer -> Integer -> Integer
bitcount x y = (x `xor` y) .&. (2^16-1)

p :: Parser (M.Map Char Integer)
p = M.fromList <$> (line `sepEndBy` char '\n')

line = do string "Generator "
          name <- oneOf ['A','B']
          string " starts with "
          start <- int
          return (name,start)

int :: Parser Integer
int = do change <- option id (negate <$ char '-')
         fromInteger . change <$> L.integer

main :: IO ()
main = do
  input <- TIO.readFile "src/Y2017/input15"
  case parse p "input15" input of
    Left err -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right bi -> do
      tprint $ parta bi

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show

-- 3999969
-- 39999694
