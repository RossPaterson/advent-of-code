module Main where

import Regex
import Utilities

-- Input processing

type Input = String

parse :: String -> Input
parse = id

-- Part One

solve1 :: Input -> Int
solve1 input =
    sumPairs $ map snd $ fst $ matchAll (regex pattern1) input

-- takes a list of matches of the form ["mul(123,456)", "123", "456"]
sumPairs :: [Matches] -> Int
sumPairs ms = sum [read s1 * read s2 | [_, s1, s2] <- ms]

regex :: String -> Regex
regex = maybe (error "bad regex") id . extendedRegex

pattern1 :: String
pattern1 = "mul\\(([0-9][0-9]?[0-9]?),([0-9][0-9]?[0-9]?)\\)"

testInput :: String
testInput = "\
    \xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 161)]

-- Part Two

solve2 :: Input -> Int
solve2 input =
    sumPairs $
    dropBrackets ["don't()"] ["do()"] $
    map snd $ fst $ matchAll (regex pattern2) input

pattern2 :: String
pattern2 = pattern1 ++ "|do\\(\\)|don't\\(\\)"

-- Drop sublists from open to close.
-- The brackets don't have to be matching.
dropBrackets :: Eq a => a -> a -> [a] -> [a]
dropBrackets open close xs =
    case span (/= open) xs of
        (front, []) -> front
        (front, _:back) -> front ++ case span (/= close) back of
            (_, []) -> []
            (_, _:xs') -> dropBrackets open close xs'

testInput2 :: String
testInput2 = "\
    \xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))\n\
    \"

tests2 :: [(String, Int)]
tests2 = [(testInput2, 48)]

main :: IO ()
main = do
    s <- readFile "input/03.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
