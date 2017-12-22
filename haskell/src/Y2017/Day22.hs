{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE BangPatterns #-}
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

data Node = Clean | Weakened | Infect | Flagged deriving (Show, Eq)

data Action = Cleaning | Infecting deriving (Show, Eq)

data Dir = U | L | R | D deriving (Show, Eq)
data Pos = Pos (Int,Int) Dir deriving (Show, Eq)


-- 5411
-- 5567
parta x = foldl1' (+) . take (10^7) . walk $ ss
  where
    s = setup x
    ss = seq s s

setup xs = (pos, board)
  where
    y = length xs `div` 2
    x = length (xs!!y) `div` 2
    pos = Pos (x,y) U
    indexed = [(ix,iy,x) | (iy, y) <- zip [0..] xs, (ix, x) <- zip [0..] y]
    board = foldl' (\m (x,y,n) -> M.insert (x,y) n m) M.empty indexed

data Turn = TurnL | TurnR | Opposite | None


walk (Pos !p !d, !b) = case M.findWithDefault Clean p b of
                          Clean ->    0:walk (newpos TurnL   , M.insert p Weakened b)
                          Weakened -> 1:walk (newpos None    , M.insert p Infect   b)
                          Infect ->   0:walk (newpos TurnR   , M.insert p Flagged  b)
                          Flagged ->  0:walk (newpos Opposite, M.insert p Clean    b)
  where
    newpos TurnL =    Pos (move p (turnleft  d)) (turnleft  d)
    newpos TurnR =    Pos (move p (turnright d)) (turnright d)
    newpos None  =    Pos (move p            d)             d
    newpos Opposite = Pos (move p (opposite  d))  (opposite d)

turnleft U = L
turnleft L = D
turnleft D = R
turnleft R = U
turnright = turnleft . turnleft . turnleft
opposite = turnleft . turnleft
move (x,y) U = (x,y-1)
move (x,y) D = (x,y+1)
move (x,y) L = (x-1,y)
move (x,y) R = (x+1,y)


p :: Parser [[Node]]
p = line `sepEndBy` char '\n'


line = some $ (Clean <$ char '.') <|> (Infect <$ char '#')

main :: IO ()
main = do
  input <- TIO.readFile "src/Y2017/input22"
  case parse p "input22" input of
    Left err -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right bi -> do
      tprint $ parta bi

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
