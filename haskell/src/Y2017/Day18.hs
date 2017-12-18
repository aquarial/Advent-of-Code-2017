{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Y2017.Day18 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text  (Parser)

import           Data.List
import qualified Data.Map.Strict       as M
import qualified Data.HashSet          as S
import qualified Data.Graph            as G

parta xs = walk M.empty M.empty [] xs

walk m rec prev []               = []
walk m rec prev (i@(Snd a)  :xs)   = walk m (M.insert a (value m (Reg a)) rec) (i:prev) xs
walk m rec prev (i@(Set a b):xs) = walk (M.insert a (value m b) m) rec (i:prev) xs
walk m rec prev (i@(Add a b):xs) = walk (M.adjust (+(value m b)) a m) rec (i:prev) xs
walk m rec prev (i@(Mod a b):xs) = walk (M.adjust (`mod`(value m b)) a m) rec (i:prev) xs
walk m rec prev (i@(Mul a b):xs) = walk (M.adjust (*(value m b)) a m) rec (i:prev) xs
walk m rec prev (i@(Rcv a)  :xs)   = if value m (Reg a) /= 0 then (i,M.lookup a rec, M.lookup a m, rec):[] else walk m rec (i:prev) xs
walk m rec prev (i@(Jgz a b):xs) = let aVal = value m a
                                       v = value m b
                                   in if aVal > 0
                                      then walk m rec (i:prev) xs
                                      else if v < 0
                                           then walk m rec (drop (-v) prev) (reverse (take (-v) prev) ++ [i]++ xs)
                                           else walk m rec (reverse (take (v-1) xs) ++ [i] ++ prev) (drop (v-1) xs)

value :: M.Map Char Int -> Val -> Int
value m (Reg c) = M.findWithDefault 0 c m
value m (Number n) = n

data Val = Reg Char | Number Int deriving Show

data Instr = Snd Char | Set Char Val | Add Char Val | Mul Char Val | Mod Char Val | Rcv Char | Jgz Val Val
  deriving Show


p :: Parser [Instr]
p = parseinstr `sepEndBy` char '\n'

parseinstr = Snd <$> (string "snd " *> letterChar) <|>
             Set <$> (string "set " *> letterChar) <*> (space *> pval) <|>
             Add <$> (string "add " *> letterChar) <*> (space *> pval) <|>
             Mul <$> (string "mul " *> letterChar) <*> (space *> pval) <|>
             Mod <$> (string "mod " *> letterChar) <*> (space *> pval) <|>
             Rcv <$> (string "rcv " *> letterChar) <|>
             Jgz <$> (string "jgz " *> pval) <*> (space *> pval)

pval = Number <$> int <|> Reg <$> letterChar

word :: Parser Text
word = T.pack <$> some letterChar

int :: Parser Int
int = do change <- option id (negate <$ char '-')
         fromInteger . change <$> L.integer


main :: IO ()
main = do
  input <- TIO.readFile "src/Y2017/input18"
  case parse p "input18" input of
    Left err -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right bi -> do
      tprint $ parta bi

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
