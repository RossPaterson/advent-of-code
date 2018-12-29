module Main where

import Utilities
import Parser

-- Input processing

type Input = [Claim]

data Claim = Claim Int Square Int Int
  deriving Show

data Square = Square Int Int
  deriving (Show, Eq, Ord)

parse :: String -> Input
parse = map (runParser claim) . lines
  where
    claim = Claim <$ char '#' <*> nat <* string " @ " <*>
        square <* string ": " <*> nat <* char 'x' <*> nat
    square = Square <$> nat <* char ',' <*> nat

-- Part One

solve1 :: Input -> Int
solve1 = length . overlaps

-- squares inside two or more claims
overlaps :: [Claim] -> [Square]
overlaps cs = [s | (s, n) <- frequency (concatMap squares cs), n > 1]

-- squares inside a claim
squares :: Claim -> [Square]
squares (Claim _ (Square left top) w h) =
    [Square (left + x) (top + y) | x <- [0..w-1], y <- [0.. h-1]]

testInput = "\
\#1 @ 1,3: 4x4\n\
\#2 @ 3,1: 4x4\n\
\#3 @ 5,5: 2x2\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 4)]

-- Part Two

solve2 :: Input -> Int
solve2 cs = head [i | Claim i _ _ _ <- nonoverlapping cs]

-- claims that do not overlap with any other
nonoverlapping :: [Claim] -> [Claim]
nonoverlapping cs = [c | c <- cs, not (any (inside c) bad)]
  where
    bad = overlaps cs

-- Is the square inside the claim?
inside :: Claim -> Square -> Bool
inside (Claim _ (Square left top) w h) (Square x y) =
    left <= x && x < left + w && top <= y && y < top + h

tests2 :: [(String, Int)]
tests2 = [(testInput, 3)]

main :: IO ()
main = do
    s <- readFile "input/03.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
