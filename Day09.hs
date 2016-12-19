module Main where

import Parser
import Control.Applicative
import Data.Char

data Element = Single Char | Repeat Int String
type Input = [Element]

parse :: String -> Input
parse = parseString . filter (not . isSpace)

parseString :: String -> Input
parseString = runParser $ many (marker <|> single)
  where
    marker = do
        char '('
        len <- nat
        char 'x'
        n <- nat
        char ')'
        str <- count len anyChar
        return (Repeat n str)
    single = Single <$> satisfy (/= '(')

expand :: Input -> String
expand = concatMap expandElement
  where
    expandElement (Single c) = [c]
    expandElement (Repeat n s) = concat (replicate n s)

solve1 :: Input -> Int
solve1 = length . expand

tests = ["ADVENT", "A(1x5)BC", "(3x3)XYZ", "A(2x2)BCD(2x2)EFG", "(6x1)(1x3)A", "X(8x2)(3x3)ABCY"]

-- Part Two --

expand2 :: Input -> Int
expand2 = sum . map expandElement
  where
    expandElement (Single c) = 1
    expandElement (Repeat n s) = n * expand2 (parseString s)

solve2 :: Input -> Int
solve2 = expand2

tests2 = ["(3x3)XYZ", "X(8x2)(3x3)ABCY", "(27x12)(20x12)(13x14)(7x10)(1x12)A", "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"]

main :: IO ()
main = do
    s <- readFile "input09.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
