{-# LANGUAGE OverloadedStrings #-}
module Y2016D01 where

import           Data.Text             (Text)
import qualified Data.Text             as T

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text  (Parser)

data Turn  = TurnL | TurnR       deriving (Show, Eq)
data Instr = Instr Turn Integer  deriving (Show, Eq)

part1 = case parse instrs "Part 1" input of
          Left err -> error $ show err
          Right is -> 1

instrs :: Parser [Instr]
instrs = sepBy instr (string ", ")
  where
    instr :: Parser Instr
    instr = Instr <$> turn <*> L.integer

    turn :: Parser Turn
    turn = (char 'L' *> pure TurnL) <|> (char 'R' *> pure TurnR)


input :: Text
input = "R1, R3, L2, L5, L2, L1, R3, L4, R2, L2, L4, R2, L1, R1, L2, R3, L1, L4, R2, L5, R3, R4, L1, R2, L1, R3, L4, R5, L4, L5, R5, L3, R2, L3, L3, R1, R3, L4, R2, R5, L4, R1, L1, L1, R5, L2, R1, L2, R188, L5, L3, R5, R1, L2, L4, R3, R5, L3, R3, R45, L4, R4, R72, R2, R3, L1, R1, L1, L1, R192, L1, L1, L1, L4, R1, L2, L5, L3, R5, L3, R3, L4, L3, R1, R4, L2, R2, R3, L5, R3, L1, R1, R4, L2, L3, R1, R3, L4, L3, L4, L2, L2, R1, R3, L5, L1, R4, R2, L4, L1, R3, R3, R1, L5, L2, R4, R4, R2, R1, R5, R5, L4, L1, R5, R3, R4, R5, R3, L1, L2, L4, R1, R4, R5, L2, L3, R4, L4, R2, L2, L4, L2, R5, R1, R4, R3, R5, L4, L4, L5, L5, R3, R4, L1, L3, R2, L2, R1, L3, L5, R5, R5, R3, L4, L2, R4, R5, R1, R4, L3"
