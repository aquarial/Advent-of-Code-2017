{-# LANGUAGE OverloadedStrings #-}
module Y2017.Day20 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Text.Megaparsec
import qualified Text.Megaparsec.Lexer as L
import           Text.Megaparsec.Text  (Parser)

import           Data.List
import           Data.Function (on)
import qualified Data.Map.Strict       as M
import qualified Data.HashSet          as S
import qualified Data.Graph            as G

parta xs = fst $ minimumBy (compare `on` manhatdist . _acl . snd) xs
  where
    manhatdist (x,y,z) = abs x + abs y + abs z

data Particle = Particle { _pos :: (Int, Int, Int)
                         , _vel :: (Int, Int, Int)
                         , _acl :: (Int, Int, Int) } deriving (Show, Eq)

p :: Parser [(Int, Particle)]
p = zip [0..] <$> parseparticle `sepEndBy` char '\n'

parseparticle :: Parser Particle
parseparticle = do string "p="
                   p <- threenum
                   string ", v="
                   v <- threenum
                   string ", a="
                   a <- threenum
                   return $ Particle p v a
  where
    threenum = (,,) <$> (char '<' *> int) <*> (char ',' *> int) <*> (char ',' *> int) <* char '>'

int :: Parser Int
int = do change <- option id (negate <$ char '-')
         fromInteger . change <$> L.integer


main :: IO ()
main = do
  input <- TIO.readFile "src/Y2017/input20"
  case parse p "input20" input of
    Left err -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right bi -> do
      tprint $ parta bi

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
