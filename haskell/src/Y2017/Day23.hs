{-# LANGUAGE OverloadedStrings #-}
module Y2017.Day23 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text  (Parser)

import           Data.List
import           Data.Numbers.Primes
import qualified Data.Map.Strict       as M
import qualified Data.HashSet          as S
import qualified Data.Graph            as G
import qualified Data.Vector           as V

--  parta = map head . group . map (M.lookup 'h') . runprogram 0 (M.insert 'a' 1 M.empty)
--  parta = length . filter isMul . runprogram 0 M.empty
-- 1455, 86

parta = unlines . take 1000 . map show . runprogram 0 (M.insert 'a' 1 M.empty)

partb = let numPrimes = length $ takeWhile (<e) $ filter (\n -> (n-b)`mod`17==0) $ dropWhile (<b) primes
            numNums   = (e - b) `div` 17 + 1
        in numNums - numPrimes
  where
    b=105700
    e=122700


runprogram i vs instrs = if i < 0 || i >= V.length instrs
                         then []
                         else vs : runprogram (nextindex instr) (vars instr) instrs
  where
    nextindex (Jnz v0 v1) | val v0 /= 0 = i + val v1
    nextindex _                         = i+1

    vars (Set c v) = M.insert c (val v) vs
    vars (Sub c v) = M.insert c (val (Reg c) - val v) vs
    vars (Mul c v) = M.insert c (val (Reg c) * val v) vs
    vars (Jnz _ _) = vs

    instr = instrs V.! i
    val (Reg c) = M.findWithDefault 0 c vs
    val (Number i) = i


data Val = Reg Char | Number Int deriving Show

data Instr = Set Char Val | Sub Char Val | Mul Char Val | Jnz Val Val
  deriving Show

p :: Parser (V.Vector Instr)
p = V.fromList <$> (parseinstr `sepEndBy` char '\n') <* eof

parseinstr = Set <$> (string "set " *> letterChar) <*> (space *> pval) <|>
             Sub <$> (string "sub " *> letterChar) <*> (space *> pval) <|>
             Mul <$> (string "mul " *> letterChar) <*> (space *> pval) <|>
             Jnz <$> (string "jnz " *> pval) <*> (space *> pval)

pval = Number <$> int <|> Reg <$> letterChar

int :: Parser Int
int = do change <- option id (negate <$ char '-')
         fromInteger . change <$> L.integer

main :: IO ()
main = do
  input <- TIO.readFile "src/Y2017/input23"
  case parse p "input23" input of
    Left err -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right bi -> do
      --putStr $ parta bi
      tprint $ partb

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
