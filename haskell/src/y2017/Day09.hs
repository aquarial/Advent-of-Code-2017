{-# LANGUAGE OverloadedStrings #-}
module Day09 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text  (Parser)

import           Data.List             hiding (group)
import qualified Data.Map.Strict       as M
import qualified Data.Set              as S

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show

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

p = group <|> garbage

data Content = Group [Content] | Garbage [Content] | C Char | NA Char
  deriving Show


group :: Parser Content
group = do char '{'
           parts <- many $ (NA <$> (char '!' >> anyChar)) <|> group
                                                          <|> garbage
                                                          <|> C <$> satisfy (/= '}')
           char '}'
           pure $ Group parts

garbage :: Parser Content
garbage = do char '<'
             parts <- many $ (NA <$> (char '!' >> anyChar)) <|> C <$> satisfy (/= '>')
             char '>'
             pure $ Garbage parts

--partA = walk 1
isChar (C _) = True
isChar _     = False

partA (Group   xs) = sum (map partA (filter (not . isChar) xs ))
partA (Garbage xs) = sum (map partA xs)
partA (C _)  = 1
partA (NA _) = 0

walk :: Int -> Content -> Int
walk n (Group xs) = n + sum (map (walk (n+1)) xs)
walk n _          = 0
