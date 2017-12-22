{-# LANGUAGE OverloadedStrings #-}
module Y2017.Day22 where

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

data Node = Clean | Infect deriving (Show, Eq)

data Action = Cleaning | Infecting deriving (Show, Eq)

data Dir = U | L | R | D deriving (Show, Eq)
data Pos = Pos (Int,Int) Dir deriving (Show, Eq)

parta = length . filter (==Infecting) . take (10^4) . walk . setup

setup xs = (pos, board)
  where
    y = length xs `div` 2
    x = length (xs!!y) `div` 2
    pos = Pos (x,y) U
    indexed = [(ix,iy,x) | (iy, y) <- zip [0..] xs, (ix, x) <- zip [0..] y]
    board = foldl' (\m (x,y,n) -> M.insert (x,y) n m) M.empty indexed

data Turn = TurnL | TurnR

walk (Pos p d, b) = case M.findWithDefault Clean p b of
                          Clean  -> Infecting : walk (newpos TurnL, M.adjust (const Infect) p b)
                          Infect -> Cleaning  : walk (newpos TurnR, M.adjust (const Clean ) p b)
  where
    newpos TurnL = Pos (move p (turnleft  d)) (turnleft  d)
    newpos TurnR = Pos (move p (turnright d)) (turnright d)
    turnleft U = L
    turnleft L = D
    turnleft D = R
    turnleft R = U
    turnright = turnleft . turnleft . turnleft
move (x,y) U = (x,y-1)
move (x,y) D = (x,y+1)
move (x,y) L = (x-1,y)
move (x,y) R = (x+1,y)


p :: Parser [[Node]]
p = line `sepEndBy` char '\n'


line = do many $ (Clean <$ char '.') <|> (Infect <$ char '#')

main :: IO ()
main = do
  input <- TIO.readFile "src/Y2017/input22"
  case parse p "input22" input of
    Left err -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right bi -> do
      tprint $ parta bi

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
