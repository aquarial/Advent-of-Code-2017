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
import qualified Data.Vector           as V

parta instrs = let p0 = program 0 instrs p1
                   p1 = program 1 instrs p0
               in length $ filter isSend p1

data Network = Send Int | Recieve deriving Show

isSend (Send i) = True
isSend _        = False


program :: Int -> V.Vector Instr -> [Network] -> [Network]
program name instrs = walk (M.fromList [('p',name)]) 0
  where
    walk vars i inputs = if i >= V.length instrs
                         then []
                         else
                           case instrs V.! i of
                             Snd a   -> Send (value vars a) : walk vars (i+1) (takeWhile isSend inputs ++ tail (dropWhile isSend inputs))
                             Set a b -> walk (M.insert a (value vars b) vars)        (i+1) inputs
                             Add a b -> walk (M.adjust (+    (value vars b)) a vars) (i+1) inputs
                             Mod a b -> walk (M.adjust (`mod`(value vars b)) a vars) (i+1) inputs
                             Mul a b -> walk (M.adjust (*    (value vars b)) a vars) (i+1) inputs
                             Jgz a b -> if value vars a > 0 then walk vars (i + value vars b) inputs else walk vars (i+1) inputs
                             Rcv a   -> Recieve : case inputs of
                                                    (Recieve) :rest -> []
                                                    (Send val):rest -> walk (M.insert a val vars) (i+1) rest
                                                    _               -> []

value :: M.Map Char Int -> Val -> Int
value m (Reg c) = M.findWithDefault 0 c m
value m (Number n) = n

data Val = Reg Char | Number Int deriving Show

data Instr = Snd Val | Set Char Val | Add Char Val | Mul Char Val | Mod Char Val | Rcv Char | Jgz Val Val
  deriving Show


p :: Parser (V.Vector Instr)
p = V.fromList <$> (parseinstr `sepEndBy` char '\n')

parseinstr = Snd <$> (string "snd " *> pval) <|>
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
