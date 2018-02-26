{-# LANGUAGE OverloadedStrings #-}
module Y2017.Day15 where

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Data.Bits
import           Data.List


data Gen = Gen { _start  :: !Int
               , _mult   :: !Int
               , _filter :: !Int }

generator :: Gen -> [Int]
generator g = filter ((==0) . (`rem` f)) $ tail $ iterate (\x -> x*m `rem` 2147483647) s
  where
    s = _start g
    m = _mult g
    f = _filter g

part1 :: (Gen, Gen) -> Int
part1 (a,b) = length $ filter (==0) $ take (40*10^6) $ zipWith bitcount ga gb
  where
    ga = generator $ a { _filter = 1 }
    gb = generator $ b { _filter = 1}

part2 :: (Gen, Gen) -> Int
part2 (a,b) = length $ filter (==0) $ take ( 5*10^6) $ zipWith bitcount ga gb
  where
    ga = generator a
    gb = generator b

bitcount :: Int -> Int -> Int
bitcount x y = (x `xor` y) .&. (2^16-1)

type Parser = Parsec Void Text

p :: Parser (Gen, Gen)
p = (,) <$> parseGen <* char '\n' <*> parseGen

parseGen :: Parser Gen
parseGen = do string "Generator "
              name <- oneOf ['A','B']
              string " starts with "
              start <- int
              pure $ if name == 'A' then Gen start 16807 4 else Gen start 48271 8

int :: Parser Int
int = do change <- option id (negate <$ char '-')
         fromInteger . change <$> L.decimal

main :: IO ()
main = do
  input <- TIO.readFile "src/Y2017/input15"
  case parse p "input15" input of
    Left err -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right bi -> do
      tprint $ part1 bi
      tprint $ part2 bi

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
