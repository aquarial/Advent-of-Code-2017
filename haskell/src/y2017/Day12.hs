{-# LANGUAGE OverloadedStrings #-}
module Day12 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text  (Parser)

import           Data.List
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S



type Comm = (Int, [Int])

p :: Parser [Comm]
p = line `sepBy` char '\n'

line = do
  name <- int
  string " <-> "
  targets <- int `sepBy` (string (", "::String))
  pure (name, targets)

int :: Parser Int
int = do
      change <- option id (negate <$ char '-')
      fromInteger . change <$> L.integer


partA = last . take 200000 . walk (S.fromList [0])


walk s ((n,ts):xs) = if S.member n s || any (\x -> S.member x s) ts
                     then S.size s : walk (S.union s (S.fromList (n:ts))) (xs++[(n,ts)])
                     else S.size s : walk s (xs++[(n,ts)])




main :: IO ()
main = do
  input <- TIO.readFile "src/y2017/input12"
  case parse p "input12" input of
    Left  err   -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right betterInput -> do
      tprint $ partA betterInput
  return ()


test :: Text -> IO ()
test input = case parse p "test" input of
               Left  err -> TIO.putStr $ T.pack $ parseErrorPretty err
               Right bi  -> tprint $ partA bi


tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
