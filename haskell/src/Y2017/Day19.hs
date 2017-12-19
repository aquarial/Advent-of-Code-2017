{-# LANGUAGE OverloadedStrings #-}
module Y2017.Day19 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Text.Megaparsec
import           Text.Megaparsec.Text  (Parser)

import           Data.List
import           Data.Maybe
import qualified Data.Vector           as V

data Dir = U | R | L | D deriving (Show, Eq)

data Piece = V | H | S | Plus | Letter Char deriving (Show, Eq)


solve :: V.Vector (V.Vector Piece) -> (Int, [Char])
solve xs = let path = walk xs (findstart xs) D
           in (length path, catMaybes (map letterToMaybe path))


letterToMaybe (Letter c) = Just c
letterToMaybe _          = Nothing


walk :: V.Vector (V.Vector Piece) -> (Int, Int) -> Dir -> [Piece]
walk vec start startDir = walkToPlus start startDir
  where
    inRange (x,y) = (0<=x)&&(0<=y)&&(x<V.length (vec V.! 0))&&(y<V.length vec)

    valid p = inRange p && get vec p == S

    changeDir :: (Int, Int) -> Dir -> [Piece]
    changeDir p d = let newdir = head $ filter (valid . move p) $ filter (/= opposite d) [U,D,L,R]
                    in get vec p : walkToPlus (move p newdir) newdir

    walkToPlus :: (Int, Int) -> Dir -> [Piece]
    walkToPlus p d = case get vec p of
                       S        -> []
                       Plus     -> changeDir p d
                       x -> x : walkToPlus (move p d) d

get :: V.Vector (V.Vector a) -> (Int, Int) -> a
get vec (x,y) = vec V.! y V.! x

opposite U = D
opposite R = L
opposite L = R
opposite D = U

move (x,y) U = (x,y-1)
move (x,y) R = (x+1,y)
move (x,y) L = (x-1,y)
move (x,y) D = (x,y+1)

findstart :: V.Vector (V.Vector Piece) -> (Int, Int)
findstart xs = case findIndex (\x -> get xs (x,0) == V ) [0..] of
                 Just x -> (x,0)
                 Nothing -> error "Nope"


toarray :: [[a]] -> V.Vector (V.Vector a)
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
      let (one,two) = solve bi
      tprint one
      tprint two

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
