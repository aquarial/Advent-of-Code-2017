{-# LANGUAGE OverloadedStrings #-}
module Y2017.Day25 where

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
import qualified Data.Vector           as V

parta (start,diag,instrs) = runturning start diag instrs 0 M.empty

runturning state 0    instrs pos tape = length $ filter (==1) $ M.elems tape
runturning state diag instrs pos tape =
  let valfun = if M.findWithDefault 0 pos tape == 0 then _zero else _one
      addDir L = subtract 1
      addDir R = (+1)
  in case valfun <$> M.lookup state instrs of
       Nothing -> error $ concat ["Could not find state ",show state," in ",show instrs]
       Just (Action v d s) -> runturning s (diag-1) instrs (addDir d pos) (M.insert pos v tape)

data Dir = L | R deriving (Show,Eq)

data Action = Action { _newval :: Int
                     , _move   :: Dir
                     , _state  :: Char }
  deriving (Show, Eq)

data TurningInstr = TurningInstr { _char :: Char
                                 , _zero :: Action
                                 , _one  :: Action }
  deriving (Show, Eq)

p :: Parser (Char, Int, M.Map Char TurningInstr)
p = do (start,d) <- preamble
       instrs <- many parseinstr
       eof
       pure (start,d, M.fromList (zip (map _char instrs) instrs))

preamble = do s <- string "Begin in state " *> anyChar <* string ".\n"
              d <- string "Perform a diagnostic checksum after " *> int <* string " steps."
              space
              pure (s,d)

parseinstr = do s <- string "In state " *> anyChar <* string ":\n"
                space *> string "If the current value is 0:\n"
                a0 <- parseaction
                space *> string "If the current value is 1:\n"
                a1 <- parseaction
                space
                pure $ TurningInstr s a0 a1

parseaction =  do v <- space *> string "- Write the value " *> int <* string ".\n"
                  d <- space *> string "- Move one slot to the " *> parsedir <* string ".\n"
                  n <- space *> string "- Continue with state " *> anyChar <* string ".\n"
                  pure $ Action v d n

parsedir = L <$ string "left" <|> R <$ string "right"

word :: Parser Text
word = T.pack <$> some letterChar

int :: Parser Int
int = do change <- option id (negate <$ char '-')
         fromInteger . change <$> L.integer


main :: IO ()
main = do
  input <- TIO.readFile "src/Y2017/input25"
  case parse p "input25" input of
    Left err -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right bi -> do
      tprint $ parta bi

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
