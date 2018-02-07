{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Y2017.Day21 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer  as L

import           Debug.Trace
import           GHC.Generics
import           Data.List
import qualified Data.Map as M
import qualified Data.HashGraph.Strict as G
import qualified Data.HashSet as S
import qualified Data.Hashable as Hashable

start :: Pattern
start = case parse ppat "" ".#./..#/###" of
          Left e -> error "formatting"
          Right x -> x

evalGraph :: [(Int, Pattern)] -> Int
evalGraph ps = sum $ map (\(i,p) -> i * numLights p) ps
  where
    numLights = length . filter (==On) . concat

walkRuleGraphN :: G.Gr Int Pattern -> Int -> [(Int, Pattern)] -> [(Int, Pattern)]
walkRuleGraphN gr n rs = head $ drop n $ iterate (walkRuleGraph gr) rs

walkRuleGraph :: G.Gr Int Pattern -> [(Int, Pattern)] -> [(Int, Pattern)]
walkRuleGraph gr ps = reduceRuleGraph (concatMap (nextRules gr) ps)

buildRuleGraph :: [Rule] -> G.Gr Int Pattern
buildRuleGraph rs = G.mkGraph edgs (map fst rs)
  where
    edgs = [ G.Edge (fst r) w r2 | r <- rs
                                 , length (fst r) == 3
                                 , (w,r2) <- generateRuleWeights rs (fst r)]

generateRuleWeights :: [Rule] -> Pattern -> [(Int, Pattern)]
generateRuleWeights rs r = count $ map (fst . applyRule rs) $ breakup (iteration rs (iteration rs (iteration rs r)))
  where
    count :: Eq a => [a] -> [(Int, a)]
    count [] = []
    count (x:xs) = (1+length (filter (==x) xs), x):count (filter (/=x) xs)

reduceRuleGraph :: [(Int, Pattern)] -> [(Int, Pattern)]
reduceRuleGraph [] = []
reduceRuleGraph ((i,p):ps) = let (same,rest) = partition ((==) p . snd) ps
                             in (i+sum (map fst same), p): reduceRuleGraph ps

nextRules :: G.Gr Int Pattern -> (Int, Pattern) -> [(Int, Pattern)]
nextRules gr (i,p) = case gr G.!? p of
                       Nothing -> trace (show p) []
                       Just ctx -> map toTuple $ S.toList $ G.tails ctx
  where
    toTuple (G.Tail w r2) = (i*w, r2)


part1 = lightson 6

part2 rs = evalGraph $ walkRuleGraphN (buildRuleGraph rs) 6 [(1,fst (applyRule rs start))]




lightson ::  Int -> [Rule] -> Int
lightson n rs = numLights . head . drop n $ iterate (iteration rs) start
  where
    numLights = length . filter (==On) . concat

iteration :: [Rule] -> Pattern -> Pattern
iteration rules = joinParts . map (snd . applyRule rules) . breakup

applyRule :: [Rule] -> Pattern -> Rule
applyRule rules xs = case find (\r -> any ((==) (fst r)) (allChanges xs) ) rules of
                       Nothing   -> error $ "Couldn't match " ++ show xs
                       Just rule -> rule

joinParts :: [[[Light]]] -> Pattern
joinParts xss = concat $ map createRow $ chunk size xss
  where
    size = round $ sqrt $ fromIntegral $ length xss
    createRow xs = [ concatMap (!!i) xs  | i <- [0..length(xs!!0)-1]]

chunk n [] = []
chunk n xs = take n xs : chunk n (drop n xs)

breakup :: Pattern -> [Pattern]
breakup xs = [blocks xs size x0 y0 | y0 <- [0,size..length xs - size]
                                   , x0 <- [0,size..length xs - size]]
  where
    size = case length xs `mod` 2 of
             0 -> 2
             1 -> 3

blocks :: [[a]] -> Int -> Int -> Int -> [[a]]
blocks xs n x0 y0 = [[xs !! (y0+y) !! (x0+x) | x <- [0..n-1]] | y <- [0..n-1]]

allChanges :: Pattern -> [Pattern]
allChanges xs = [f0 (f1 xs) | f0 <- [id, flipV]
                            , f1 <- [id, rotate, rotate.rotate, rotate.rotate.rotate] ]

flipV = reverse

rotate = flipV . transpose

data Light = On | Off deriving (Show, Eq, Ord, Generic, Hashable.Hashable)
type Pattern = [[Light]]
type Rule = (Pattern, Pattern)

type Parser = Parsec Void Text

p :: Parser [Rule]
p = line `sepEndBy` char '\n'

line :: Parser Rule
line = do p0 <- ppat
          string " => "
          p1 <- ppat
          return (p0,p1)

ppat = prow `sepBy` char '/'
prow = many plight

plight :: Parser Light
plight = (Off <$ char '.') <|> (On <$ char '#')

main :: IO ()
main = do
  input <- TIO.readFile "src/Y2017/input21"
  case parse p "input21" input of
    Left err -> TIO.putStr $ T.pack $ parseErrorPretty err
    Right bi -> do
      tprint $ part1 bi
      tprint $ part2 bi

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show
