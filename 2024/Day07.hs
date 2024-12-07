module Main where

import Parser
import Utilities

-- Input processing

type Input = [Equation]

data Equation = Eqn { lhs :: Int, rhs :: [Int] }
    deriving Show

parse :: String -> Input
parse = map (runParser equation) . lines
  where
    equation = Eqn <$> nat <* string ": " <*> (nat `sepBy1` space)

-- Part One

solve1 :: Input -> Int
solve1 = sum . map lhs . filter (valid [(+), (*)])

type Binop = Int -> Int -> Int

-- is the equation valid with any possible insertion of the operators?
valid :: [Binop] -> Equation -> Bool
valid ops (Eqn l rs) = elem l (evaluations ops rs)

-- possible values with any possible insertion of the operators in the list
evaluations :: [Binop] -> [Int] -> [Int]
evaluations _ [] = []
evaluations ops (n:ns) = foldl combinations [n] ns
  where
    combinations rs m = [op r m | r <- rs, op <- ops]

testInput :: String
testInput = "\
    \190: 10 19\n\
    \3267: 81 40 27\n\
    \83: 17 5\n\
    \156: 15 6\n\
    \7290: 6 8 6 15\n\
    \161011: 16 10 13\n\
    \192: 17 8 14\n\
    \21037: 9 7 18 13\n\
    \292: 11 6 16 20\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 3749)]

-- Part Two

solve2 :: Input -> Int
solve2 = sum . map lhs . filter (valid [(+), (*), cat])

-- concatenate the digits of the two numbers
cat :: Int -> Int -> Int
cat m n = m*10^numDigits n + n

-- number of digits in n
numDigits :: Int -> Int
numDigits n = length $ takeWhile (<= n) $ iterate (*10) 1

tests2 :: [(String, Int)]
tests2 = [(testInput, 11387)]

main :: IO ()
main = do
    s <- readFile "input/07.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
