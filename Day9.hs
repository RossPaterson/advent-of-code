module Day9 where

import Parser
import Control.Applicative
import Data.Char

data Marker = End | Single Char String | Marker Int String String

parseString :: String -> Marker
parseString = runParser $
    marker <$ char '(' <*> nat <* char 'x' <*> nat <* char ')' <*> rest <|>
    Single <$> satisfy (/= '(') <*> rest <|>
    pure End
  where
    marker len count s = Marker count (take len s) (drop len s)

expand :: String -> String
expand s = case parseString s of
    End -> []
    Single c s -> c : expand s
    Marker count prefix s -> concat (replicate count prefix) ++ expand s

solve1 :: String -> Int
solve1 = length . expand . filter (not . isSpace)

tests = ["ADVENT", "A(1x5)BC", "(3x3)XYZ", "A(2x2)BCD(2x2)EFG", "(6x1)(1x3)A", "X(8x2)(3x3)ABCY"]

-- Part Two --

expand2 :: String -> Int
expand2 s = case parseString s of
    End -> 0
    Single c s -> 1 + expand2 s
    Marker count prefix s -> count * expand2 prefix + expand2 s

solve2 :: String -> Int
solve2 = expand2 . filter (not . isSpace)

tests2 = ["(3x3)XYZ", "X(8x2)(3x3)ABCY", "(27x12)(20x12)(13x14)(7x10)(1x12)A", "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"]

puzzle1 = do
    s <- readFile "input9.txt"
    print (solve1 s)

puzzle2 = do
    s <- readFile "input9.txt"
    print (solve2 s)
