{-# LANGUAGE OverloadedStrings #-}
module Y2017.Day19 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text  (Parser)

import           Data.List
import           Data.Maybe
import qualified Data.Map.Strict       as M
import qualified Data.HashSet          as S
import qualified Data.Graph            as G
import qualified Data.Vector           as V

data Dir = U | R | L | D deriving (Show, Eq)
data Piece = V | H | S | Plus | Letter Char deriving (Show, Eq)

letterToMaybe (Letter c) = Just c
letterToMaybe _          = Nothing

parta xs = length $ walk xs (findstart xs) D

--walk :: V.Vector (V.Vector Piece) -> S.HashSet (Int,Int) -> (Int,Int) -> [Piece]
walk vec start startDir = walkToPlus start startDir
  where
    walkable S = False
    walkable _ = True

    inRange (x,y) = (0<=x)&&(0<=y)&&(x<V.length (vec V.! 0))&&(y<V.length vec)

    changeDir :: (Int, Int) -> Dir -> [Piece]
    changeDir p d = let newdir = head $ filter (\d -> inRange (move p d) && walkable (get vec (move p d))) $ filter (/= opposite d) [U,D,L,R]
                    in get vec p : walkToPlus (move p newdir) newdir

    walkToPlus :: (Int, Int) -> Dir -> [Piece]
    walkToPlus p d = case get vec p of
                       S        -> []
                       Plus     -> changeDir p d
                       x -> x : walkToPlus (move p d) d

get vec (x,y) = vec V.! y V.! x

opposite U = D
opposite R = L
opposite L = R
opposite D = U

move (x,y) U = (x,y-1)
move (x,y) R = (x+1,y)
move (x,y) L = (x-1,y)
move (x,y) D = (x,y+1)

findstart xs = case findIndex (\x -> get xs (x,0) == V ) [0..] of
                 Just x -> (x,0)
                 Nothing -> error "Nope"



toarray es = V.fromList $ map V.fromList es

p :: Parser (V.Vector (V.Vector Piece))
p = toarray <$> many piece `sepEndBy` char '\n'

piece :: Parser Piece
piece = V <$ string "|"    <|>
        H <$ string "-"    <|>
        S <$ string " "    <|>
        Plus <$ string "+" <|>
        Letter <$> letterChar

main :: IO ()
main = do
  input <- TIO.readFile "src/Y2017/input19"
  case parse p "input19" input of
    Left err -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right bi -> do
      tprint $ parta bi

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
