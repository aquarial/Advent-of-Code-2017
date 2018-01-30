{-# LANGUAGE OverloadedStrings #-}
module Y2017.Day21 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer  as L

import           Data.List

start :: [[Light]]
start = case parse ppat "start" ".#./..#/###" of
          Left e -> error "formatting"
          Right x -> x

part1 xs = let lights = length . filter (==On) . concat
           in lights $ last $  take (5+1) $ iterate (iteration xs) start

part2 xs = let lights = length . filter (==On) . concat
           in lights $ last $  take (18+1) $ iterate (iteration xs) start

iteration rules = joinParts . map (applyRule rules) . breakup

applyRule rules xs = case find (\r -> any (\c -> c == fst r) (allChanges xs)) rules of
                       Nothing -> error $ "Couldn't match " ++ show (allChanges xs)
                       Just r ->  snd r

joinParts :: [[[a]]] -> [[a]]
joinParts xss = concat $ map createRow $ chunk size xss
  where
    size = round $ sqrt $ fromIntegral $ length xss
    createRow xs = [ concatMap (!!i) xs  | i <- [0..length(xs!!0)-1]]

chunk n [] = []
chunk n xs = take n xs : chunk n (drop n xs)

breakup :: [[a]] -> [[[a]]]
breakup xs = [blocks xs size x0 y0 | y0 <- [0,size..length xs - size]
                                   , x0 <- [0,size..length xs - size]]
  where size = 2 + (length xs `mod` 2)

blocks :: [[a]] -> Int -> Int -> Int -> [[a]]
blocks xs n x0 y0 = [[xs !! (y0+y) !! (x0+x) | x <- [0..n-1]] | y <- [0..n-1]]



allChanges :: Show t => [[t]] -> [[[t]]]
allChanges xs = [f0 (f1 xs) | f0 <- [flipV, flipH, id]
                            , f1 <- [id, rotate, rotate.rotate, rotate.rotate.rotate] ]

flipV xs = [[xs !! y !! x | x <- [0..length (xs !! 0)-1]] | y <- reverse [0..length xs-1]]
flipH xs = [[xs !! y !! x | x <- reverse [0..length (xs !! 0)-1]] | y <- [0..length xs-1]]
rotate xs = case length xs of
              2 -> rotate2 xs
              3 -> rotate3 xs
              n -> error $ concat ["length ", show n, " cannot be rotated in ", show xs]

rotate2 xs = map (map (get xs)) $ [ [(0,1), (0,0)], [(1,1), (1,0)]]
rotate3 xs = map (map (get xs)) $ [ [(0,2),(0,1),(0,0)]
                                  , [(1,2),(1,1),(1,0)]
                                  , [(2,2),(2,1),(2,0)] ]

get xs (x,y) = xs !! y !! x

data Light = On | Off deriving (Show, Eq)
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

int :: Parser Int
int = do change <- option id (negate <$ char '-')
         fromInteger . change <$> L.decimal


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
