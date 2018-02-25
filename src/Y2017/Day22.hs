{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Strict            #-}
{-# LANGUAGE StrictData        #-}

module Y2017.Day22 where

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as TIO

import           Data.Void
import           Text.Megaparsec            hiding (Pos)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Data.HashMap.Strict        as M
import qualified Data.HashTable.IO          as HT
import           Data.List

import           Control.Monad              (foldM, when)

data Node = Clean | Weakened | Infect | Flagged deriving (Show, Eq)

data Action = Cleaning | Infecting deriving (Show, Eq)

data Dir = U | L | R | D deriving (Show, Eq)
data Pos = Pos !(Int,Int) !Dir deriving (Show, Eq)

parta :: [[Node]] -> Int
parta = foldl1' (+) . take (10^4) . walka . setup


setup :: [[Node]] -> (Pos, M.HashMap (Int, Int) Node)
setup xs = (pos, board)
  where
    y = length xs `div` 2
    x = length (xs!!y) `div` 2
    pos = Pos (x,y) U
    indexed = [(ix,iy,x) | (iy, y) <- zip [0..] xs, (ix, x) <- zip [0..] y]
    board = foldl' (\m (x,y,n) -> M.insert (x,y) n m) M.empty indexed

data Turn = TurnL | TurnR | Opposite | None

walka :: (Pos, M.HashMap (Int, Int) Node) -> [Int]
walka (Pos !p !d, !b) = case M.lookupDefault Clean p b of
                          Clean  -> 1:walka (newpos p d TurnL, M.insert p Infect b)
                          Infect -> 0:walka (newpos p d TurnR, M.insert p Clean  b)

newpos :: (Int, Int) -> Dir -> Turn -> Pos
newpos xy d TurnL    = Pos (move xy (turnleft  d)) (turnleft  d)
newpos xy d TurnR    = Pos (move xy (turnright d)) (turnright d)
newpos xy d None     = Pos (move xy            d)             d
newpos xy d Opposite = Pos (move xy (opposite  d))  (opposite d)

turnleft :: Dir -> Dir
turnleft U = L
turnleft L = D
turnleft D = R
turnleft R = U
turnright = turnleft . turnleft . turnleft
opposite = turnleft . turnleft

move (!x,!y) U = (x,y-1)
move (!x,!y) D = (x,y+1)
move (!x,!y) L = (x-1,y)
move (!x,!y) R = (x+1,y)


type HashTable k v = HT.BasicHashTable k v

partb :: [[Node]] -> IO Int
partb ns = let (pos, hmap) = setup ns
               tlist = M.toList hmap
               ins m (k,v) = HT.insert m k v >> pure m
               o h = foldM ins h []
           in do h <- HT.new
                 foldM ins h (M.toList hmap)
                 partbwalk 0 0 pos h


partbwalk :: Int -> Int -> Pos -> HashTable (Int, Int) Node -> IO Int
partbwalk !n !i (Pos !p !d) m =
  if n == 10^7
  then pure i
  else do vmaybe <- HT.lookup m p
          let nv = case vmaybe of
                     Nothing       -> Weakened
                     Just Clean    -> Weakened
                     Just Weakened -> Infect
                     Just Infect   -> Flagged
                     Just Flagged  -> Clean
          --when (n `mod` (10^5) == 0) $ print p
          HT.insert m p nv
          case vmaybe of
            Nothing       -> partbwalk (n+1) i     (newpos p d TurnL   ) m
            Just Clean    -> partbwalk (n+1) i     (newpos p d TurnL   ) m
            Just Weakened -> partbwalk (n+1) (i+1) (newpos p d None    ) m
            Just Infect   -> partbwalk (n+1) i     (newpos p d TurnR   ) m
            Just Flagged  -> partbwalk (n+1) i     (newpos p d Opposite) m


type Parser = Parsec Void Text

p :: Parser [[Node]]
p = line `sepEndBy` char '\n'


line :: Parser [Node]
line = some $ (Clean <$ char '.') <|> (Infect <$ char '#')

main :: IO ()
main = do
  input <- TIO.readFile "src/Y2017/input22"
  case parse p "input22" input of
    Left err -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right bi -> do
      tprint $ parta bi
      partb bi >>= print

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
