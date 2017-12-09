module Day09 where

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Text.Megaparsec
import           Text.Megaparsec.Text  (Parser)

data Content = Group [Content] | Garbage [Content] | C Char | NA Char
  deriving Show


parser :: Parser Content
parser = group <|> garbage

group :: Parser Content
group = Group <$> between (char '{') (char '}') parts
  where parts = many $ group <|> garbage <|> C <$> satisfy (/= '}')

garbage :: Parser Content
garbage = Garbage <$> between (char '<') (char '>') parts
  where parts = many $ cancelled <|> C <$> satisfy (/= '>')


cancelled :: Parser Content
cancelled = NA <$> (char '!' >> anyChar)


part1 :: Content -> Int
part1 = walk 1

walk :: Int -> Content -> Int
walk n (Group xs) = n + sum (map (walk (n+1)) xs)
walk n _          = 0


part2 :: Content -> Int
part2 (Group   xs) = sum (map part2 xs)
part2 (Garbage xs) = length $ filter isChar xs
part2 _ = 0


isChar :: Content -> Bool
isChar (C _) = True
isChar _     = False



main :: IO ()
main = do
  input <- TIO.readFile "src/y2017/input09"
  case parse parser "input09" input of
    Left  err   -> tprint err
    Right betterInput -> do
      tprint $ part1 betterInput
      tprint $ part2 betterInput
  return ()

tprint :: Show a => a -> IO ()
tprint = TIO.putStrLn . T.pack . show

