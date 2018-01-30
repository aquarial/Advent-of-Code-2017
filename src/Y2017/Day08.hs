{-# LANGUAGE OverloadedStrings #-}
module Y2017.Day08 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Data.List
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show

main = do
  input <- TIO.readFile "src/Y2017/input08"
  case parse p "input08" input of
    Left  err   -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right betterInput -> do
      let (a,b) = partA betterInput
      tprint $ a
      tprint $ b
  return ()

type Instruction = (Text, Int->Int, Text, (Int->Bool))

type Parser = Parsec Void Text

p :: Parser [Instruction]
p = line `sepBy` char '\n'

line :: Parser Instruction
line = do reg <- word
          space
          change <- (+) <$ string "inc" <|> subtract <$ string "dec"
          space
          size <- int
          string " if "
          other <- word
          space
          comparer <- comparitor <$> symb
          space
          limit <- int
          pure (reg, change size, other, (`comparer` limit))

symb :: Parser Text
symb = T.pack <$> some (oneOf ("!<>="::String))

word :: Parser Text
word = T.pack <$> some letterChar

int :: Parser Int
int = do change <- option id (negate <$ char '-')
         fromInteger . change <$> L.decimal

comparitor "==" = (==)
comparitor "!=" = (/=)
comparitor ">"  = (>)
comparitor "<"  = (<)
comparitor ">=" = (>=)
comparitor "<=" = (<=)

-- type Instruction = (Text, Int->Int, Text, (Int->Bool))

partA :: [Instruction] -> (Int, Int)
partA xs = let (m, s) = walk xs (M.empty) 0
           in (maximum (M.elems m), s)

walk ::[Instruction] -> M.Map Text Int -> Int -> (M.Map Text Int, Int)
walk []                               m ma = (m, ma)
walk ((this, change, other, test):xs) m ma = walk xs (if test valother then withOp else m)  (maximum [ma, valthis, valother])
  where
    withOp   = M.insert this (change valthis) m
    valthis  = M.findWithDefault 0 this m
    valother = M.findWithDefault 0 other m
