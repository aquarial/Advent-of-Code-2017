{-# LANGUAGE OverloadedStrings #-}
module Y2017.Day20 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer  as L

import           Data.List
import           Data.Function (on)
import qualified Data.Map.Strict       as M

part1 = fst . minimumBy (compare `on` manhatdist . _acl . snd) . zip [0..]

part2 = last . take 50 . numberOfCollisions

numberOfCollisions xs = length xs : (numberOfCollisions $ removeCollisions $ map tick xs)


data Particle = Particle { _pos :: (Int, Int, Int)
                         , _vel :: (Int, Int, Int)
                         , _acl :: (Int, Int, Int) } deriving (Show, Eq)

removeCollisions :: [Particle] -> [Particle]
removeCollisions parts = filter (\p -> M.findWithDefault 1 (_pos p) (allpos parts) == 1) parts

allpos parts= foldl' (\m p -> M.insertWith (+) p 1 m) M.empty (map _pos parts)

tick :: Particle -> Particle
tick (Particle p v a) = Particle (tick3 p v2) v2 a
  where v2 = tick3 v a
        tick3 (x,y,z) (dx,dy,dz) = (x+dx,y+dy,z+dz)

manhatdist (x,y,z) = abs x + abs y + abs z

type Parser = Parsec Void Text

p :: Parser [Particle]
p = parseparticle `sepEndBy` char '\n'

parseparticle :: Parser Particle
parseparticle = Particle <$> (string   "p=" *> threenum) <*>
                             (string ", v=" *> threenum) <*>
                             (string ", a=" *> threenum)
  where
    threenum = (,,) <$> (char '<' *> int) <*> (char ',' *> int) <*> (char ',' *> int) <* char '>'

int :: Parser Int
int = do change <- option id (negate <$ char '-')
         fromInteger . change <$> L.decimal


main :: IO ()
main = do
  input <- TIO.readFile "src/Y2017/input20"
  case parse p "input20" input of
    Left err -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right bi -> do
      tprint $ part1 bi
      tprint $ part2 bi

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
