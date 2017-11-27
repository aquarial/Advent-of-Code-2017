{-# LANGUAGE OverloadedStrings #-}
module Y2016D01 where

import           Data.Text             (Text)
import qualified Data.Text             as T

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text  (Parser)

import           Data.Hashable
import qualified Data.HashSet          as Set

data Turn  = TurnL | TurnR | NoTurn deriving (Show, Eq)
data Instr = Instr !Turn !Integer   deriving (Show, Eq)

data Direction = U | R | D | L   deriving (Show, Eq, Enum)
data Position = Position { _x :: !Integer
                         , _y :: !Integer
                         , _d :: !Direction } deriving (Show)

instance Eq Position where
  p1 == p2 = _x p1 == _x p2 && _y p1 == _y p2

instance Hashable Position where
  hashWithSalt s p = (+ s) $ fromIntegral $ (_x p * 997) + _y p


part1 :: Integer
part1 = case parse instructions "Part 1" input of
          Left err -> error $ show err
          Right is -> dist $ foldl update (Position 0 0 U) is


part2 :: Integer
part2 = case parse instructions "Part 2" input of
          Left err -> error $ show err
          Right is -> dist $ walk Set.empty (Position 0 0 U) $ concatMap flatten is
  where
    walk s p []     = p
    walk s p (i:is) = if Set.member p s then p
                      else walk (Set.insert p s) (update p i) is

    flatten :: Instr -> [Instr] -- flatten one large move into many small ones
    flatten (Instr t 0) = []
    flatten (Instr t n) = Instr t 1 : flatten (Instr NoTurn (n - 1))

update :: Position -> Instr -> Position
update pos (Instr turn dist) = newPos newDir
  where
    newPos U = Position (_x pos) (_y pos + dist) newDir
    newPos D = Position (_x pos) (_y pos - dist) newDir
    newPos R = Position (_x pos + dist) (_y pos) newDir
    newPos L = Position (_x pos - dist) (_y pos) newDir

    newDir = doturn turn (_d pos)

    doturn :: Turn -> Direction -> Direction
    doturn TurnL U = L
    doturn TurnL d = pred d
    doturn TurnR L = U
    doturn TurnR d = succ d
    doturn NoTurn d = d

dist :: Position -> Integer
dist pos = abs (_x pos) + abs (_y pos)

instructions :: Parser [Instr]
instructions = sepBy instr (string ", ")
  where
    instr :: Parser Instr
    instr = Instr <$> turn <*> L.integer

    turn :: Parser Turn
    turn = (char 'L' *> pure TurnL) <|> (char 'R' *> pure TurnR)


input :: Text
input = "R1, R3, L2, L5, L2, L1, R3, L4, R2, L2, L4, R2, L1, R1, L2, R3, L1, L4, R2, L5, R3, R4, L1, R2, L1, R3, L4, R5, L4, L5, R5, L3, R2, L3, L3, R1, R3, L4, R2, R5, L4, R1, L1, L1, R5, L2, R1, L2, R188, L5, L3, R5, R1, L2, L4, R3, R5, L3, R3, R45, L4, R4, R72, R2, R3, L1, R1, L1, L1, R192, L1, L1, L1, L4, R1, L2, L5, L3, R5, L3, R3, L4, L3, R1, R4, L2, R2, R3, L5, R3, L1, R1, R4, L2, L3, R1, R3, L4, L3, L4, L2, L2, R1, R3, L5, L1, R4, R2, L4, L1, R3, R3, R1, L5, L2, R4, R4, R2, R1, R5, R5, L4, L1, R5, R3, R4, R5, R3, L1, L2, L4, R1, R4, R5, L2, L3, R4, L4, R2, L2, L4, L2, R5, R1, R4, R3, R5, L4, L4, L5, L5, R3, R4, L1, L3, R2, L2, R1, L3, L5, R5, R5, R3, L4, L2, R4, R5, R1, R4, L3"
