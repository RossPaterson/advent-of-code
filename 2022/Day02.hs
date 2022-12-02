module Main where

import Utilities
import Parser

-- Input processing

type Input = [Round]
type Round = (ABC, XYZ)
data ABC = A | B | C
    deriving (Bounded, Enum, Read, Show)
data XYZ = X | Y | Z
    deriving (Bounded, Enum, Read, Show)

parse :: String -> Input
parse s = map (runParser pair) (lines s)
  where
    pair = (,) <$> enumValue <* space <*> enumValue

-- Part One

-- Rock-Paper-Scissors game

data Play = Rock | Paper | Scissors
    deriving (Bounded, Enum, Show)

data Outcome = Lose | Draw | Win
    deriving (Bounded, Enum, Eq, Show)

outcome :: Play -> Play -> Outcome
outcome act resp = toEnum ((fromEnum resp - fromEnum act + 1) `mod` 3)

score :: Play -> Outcome -> Int
score resp out = fromEnum resp + 1 + 3*fromEnum out

recode :: (Enum a, Enum b) => a -> b
recode = toEnum . fromEnum

-- values represent opponent's play and our response
score1 :: Round -> Int
score1 (x, y) = score resp out
  where
    act = recode x
    resp = recode y
    out = outcome act resp

solve1 :: Input -> Int
solve1 = sum . map score1

testInput :: String
testInput = "\
    \A Y\n\
    \B X\n\
    \C Z\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 15)]

-- Part Two

-- response to obtain the specified outcome
response :: Play -> Outcome -> Play
-- response act out = head [r | r <- allValues, outcome act r == out]
response act out = toEnum ((fromEnum out + fromEnum act - 1) `mod` 3)

-- values represent opponent's play and game outcome
score2 :: Round -> Int
score2 (x, y) = score resp out
  where
    act = recode x
    out = recode y
    resp = response act out

solve2 :: Input -> Int
solve2 = sum . map score2

tests2 :: [(String, Int)]
tests2 = [(testInput, 12)]

main :: IO ()
main = do
    s <- readFile "input/02.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
