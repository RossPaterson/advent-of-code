module Day9 where

import Parser
import Control.Applicative
import Data.Char

data Element = Single Char | Repeat Int String
type Input = [Element]

parseString :: String -> Input
parseString = runParser $ many (marker <|> single)
  where
    marker = do
        char '('
        len <- nat
        char 'x'
        count <- nat
        char ')'
        str <- sequence (replicate len anyChar)
        return (Repeat count str)
    single = Single <$> satisfy (/= '(')

expand :: Input -> String
expand = concatMap expandElement
  where
    expandElement (Single c) = [c]
    expandElement (Repeat n s) = concat (replicate n s)

solve1 :: String -> Int
solve1 = length . expand . parseString . filter (not . isSpace)

tests = ["ADVENT", "A(1x5)BC", "(3x3)XYZ", "A(2x2)BCD(2x2)EFG", "(6x1)(1x3)A", "X(8x2)(3x3)ABCY"]

-- Part Two --

expand2 :: Input -> Int
expand2 = sum . map expandElement
  where
    expandElement (Single c) = 1
    expandElement (Repeat n s) = n * expand2 (parseString s)

solve2 :: String -> Int
solve2 = expand2 . parseString . filter (not . isSpace)

tests2 = ["(3x3)XYZ", "X(8x2)(3x3)ABCY", "(27x12)(20x12)(13x14)(7x10)(1x12)A", "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"]

puzzle1 = do
    s <- readFile "input9.txt"
    print (solve1 s)

puzzle2 = do
    s <- readFile "input9.txt"
    print (solve2 s)
