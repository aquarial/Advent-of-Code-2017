{-# LANGUAGE OverloadedStrings #-}
module Day09 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text  (Parser)

import           Control.Monad         (void)
import           Data.List             hiding (group)
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S
import           Data.Tree             as T

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show

test :: Text -> (Group, Int)
test x = case parse p "test" x of
           Left err -> error $ show err
           Right bi -> (bi, partA bi)

main = do
  input <- TIO.readFile "src/y2017/input09"
  case parse p "input09" input of
    Left  err   -> tprint err
    Right betterInput -> do
      tprint $ partA betterInput
  return ()

p = group


type Forrest = [Group]
data Group = Group Forrest | NA deriving Show

group :: Parser Group
group = do char '{'
           parts <- many $ (NA <$ (char '!' >> anyChar)) <|> group
                                                         <|> garbage
                                                         <|> NA <$ satisfy (/= '}')
           char '}'
           pure $ Group parts

garbage :: Parser Group
garbage = do char '<'
             many $ (char '!' >> anyChar) <|> satisfy (/= '>')
             char '>'
             pure NA

partA = walk 1

walk :: Int -> Group -> Int
walk n NA         = 0
walk n (Group xs) = n + sum (map (walk (n+1)) xs)
