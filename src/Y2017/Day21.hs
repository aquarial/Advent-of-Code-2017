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

import           GHC.Generics
import           Data.List
import qualified Data.Map as M
import qualified Data.HashGraph.Strict as G
import qualified Data.HashSet as S
import qualified Data.Hashable as Hashable

start :: [[Light]]
start = case parse ppat "" ".#./..#/###" of
          Left e -> error "formatting"
          Right x -> x


----evalGraph :: [(Int, Rule)] ->
--evalGraph rs = map (\(i,r) -> i * numLights (fst ))
--  where
--    numLights = length . filter (==On) . concat

walkRuleGraphN :: G.Gr Int Rule -> Int -> [(Int, Rule)] -> [(Int, Rule)]
walkRuleGraphN gr n rs = head $ drop n $ iterate (walkRuleGraph gr) rs

walkRuleGraph :: G.Gr Int Rule -> [(Int, Rule)] -> [(Int, Rule)]
walkRuleGraph gr rs = reduceRuleGraph (concatMap (nextRules gr) rs)

buildRuleGraph :: [Rule] -> G.Gr Int Rule
buildRuleGraph rs = G.mkGraph edgs rs
  where
    edgs = [ G.Edge r w (r2,undefined) | r <- rs
                                       , (w,r2) <- generateRuleWeights rs r]

generateRuleWeights :: [Rule] -> (Pattern, z) -> [(Int, Pattern)]
generateRuleWeights rs r = count $ breakup (iteration rs (iteration rs (fst r)))
  where
    count :: Eq a => [a] -> [(Int, a)]
    count [] = []
    count (x:xs) = (1+length (filter (==x) xs), x):count (filter (/=x) xs)

reduceRuleGraph :: [(Int, Rule)] -> [(Int, Rule)]
reduceRuleGraph ((i,r):rs) = let (same,rest) = partition ((==) r . snd) rs
                             in (i+sum (map fst same), r): reduceRuleGraph rs

nextRules :: G.Gr Int Rule -> (Int, Rule) -> [(Int, Rule)]
nextRules gr (i,r) = map toTuple $ S.toList $ G.tails $ gr G.! r
  where
    toTuple (G.Tail w r2) = (i*w, r2)

part1 = lightson 5

part2 = lightson 18

--iterInfo :: [Rule] -> M.Map Pattern (M.Map Pattern Int)
---iterInfo rs = map (\i -> iteration rs (iteration rs i)) $ filter ((==) 3 . length) rs
--  where
--    count r = M.insertWith (+) r

lightson ::  Int -> [ ( [[Light]] , [[Light]] ) ] -> Int
lightson n xs = numLights . head . drop n $ iterate (iteration nrules) start
  where
    nrules = concat [ zip (allChanges rule) (repeat result) |  (rule, result) <- xs]
    numLights = length . filter (==On) . concat

iteration :: [Rule] -> Pattern -> Pattern
iteration rules = joinParts . map (applyRule rules) . breakup

applyRule :: [Rule] -> Pattern -> Pattern
applyRule rules xs = case find (\r -> fst r == xs) rules of
                       Nothing -> error $ "Couldn't match " ++ show (allChanges xs)
                       Just (rule,result)  -> result

joinParts :: [[[Light]]] -> Pattern
joinParts xss = concat $ map createRow $ chunk size xss
  where
    size = round $ sqrt $ fromIntegral $ length xss
    createRow xs = [ concatMap (!!i) xs  | i <- [0..length(xs!!0)-1]]

chunk n [] = []
chunk n xs = take n xs : chunk n (drop n xs)

breakup :: [[a]] -> [[[a]]]
breakup xs = [blocks xs size x0 y0 | y0 <- [0,size..length xs - size]
                                   , x0 <- [0,size..length xs - size]]
  where
    size = case length xs `mod` 2 of
             0 -> 2
             1 -> 3

blocks :: [[a]] -> Int -> Int -> Int -> [[a]]
blocks xs n x0 y0 = [[xs !! (y0+y) !! (x0+x) | x <- [0..n-1]] | y <- [0..n-1]]

allChanges :: Show t => [[t]] -> [[[t]]]
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
