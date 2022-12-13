module Main where

import Utilities
import Parser
import Control.Applicative
import Data.List

-- Input processing

type Input = [(Packet, Packet)]
type Packet = Forest Int
type Forest a = [Tree a]
data Tree a = Leaf a | List (Forest a)
    deriving (Show)

parse :: String -> Input
parse = map (mkPair . map (runParser forest) . lines) . paragraphs
  where
    mkPair [x, y] = (x, y)
    mkPair _ = error "not a pair"
    forest = char '[' *> (pure [] <|> sepBy1 tree (char ',')) <* char ']'
    tree = Leaf <$> nat <|> List <$> forest

-- Part One

instance (Ord a) => Eq (Tree a) where
    x == y = compare x y == EQ

instance (Ord a) => Ord (Tree a) where
    compare (Leaf x1) (Leaf x2) = compare x1 x2
    compare (List ts1) (List ts2) = compare ts1 ts2
    compare (Leaf x) (List ts) = compare [Leaf x] ts
    compare (List ts) (Leaf x) = compare ts [Leaf x]

solve1 :: Input -> Int
solve1 ps = sum [n | (n, (x, y)) <- zip [1..] ps, x < y]

testInput :: String
testInput = "\
    \[1,1,3,1,1]\n\
    \[1,1,5,1,1]\n\
    \\n\
    \[[1],[2,3,4]]\n\
    \[[1],4]\n\
    \\n\
    \[9]\n\
    \[[8,7,6]]\n\
    \\n\
    \[[4,4],4,4]\n\
    \[[4,4],4,4,4]\n\
    \\n\
    \[7,7,7,7]\n\
    \[7,7,7]\n\
    \\n\
    \[]\n\
    \[3]\n\
    \\n\
    \[[[]]]\n\
    \[[]]\n\
    \\n\
    \[1,[2,[3,[4,[5,6,7]]]],8,9]\n\
    \[1,[2,[3,[4,[5,6,0]]]],8,9]\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 13)]

-- Part Two

dividers :: [Packet]
dividers = [[Leaf 2], [Leaf 6]]

concatPairs :: [(a, a)] -> [a]
concatPairs xys = [z | (x, y) <- xys, z <- [x, y]]

solve2 :: Input -> Int
solve2 ps =
    product [n |
        (n, p) <- zip [1..] (sort (dividers ++ concatPairs ps)),
        elem p dividers]

tests2 :: [(String, Int)]
tests2 = [(testInput, 140)]

main :: IO ()
main = do
    s <- readFile "input/13.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
