module Main where

import Utilities
import Data.List
import Data.Maybe

-- Input processing

type Input = [[Char]]

parse :: String -> Input
parse = lines

-- Part One

-- matching brackets
brackets :: [(Char, Char)]
brackets = [
    ('(', ')'),
    ('[', ']'),
    ('{', '}'),
    ('<', '>')]

scores1 :: [(Char, Int)]
scores1 = [
    (')', 3),
    (']', 57),
    ('}', 1197),
    ('>', 25137)]

data MatchResult
    = Correct -- all nested brackets match
    | Unexpected Char -- not an opening bracket or an expected closing bracket
    | Unmatched [Char] -- end of input reached with these not matched
    deriving Show

matchBrackets :: [Char] -> MatchResult
matchBrackets = match []
  where
    -- first parameter is a stack of expected closing brackets
    match :: [Char] -> [Char] -> MatchResult
    match [] [] = Correct
    match bs [] = Unmatched bs
    match bs (c:cs) = case lookup c brackets of
        Just b -> match (b:bs) cs
        Nothing -> case bs of
            b:bs' | c == b -> match bs' cs
            _ -> Unexpected c

solve1 :: Input -> Int
solve1 ls
    = sum [fromJust (lookup c scores1) | Unexpected c <- map matchBrackets ls]

testInput :: String
testInput = "\
    \[({(<(())[]>[[{[]{<()<>>\n\
    \[(()[<>])]({[<{<<[]>>(\n\
    \{([(<{}[<>[]}>{[]{[(<()>\n\
    \(((({<>}<{<{<>}{[]{[]{}\n\
    \[[<[([]))<([[{}[[()]]]\n\
    \[{[{({}]{}}([{[{{{}}([]\n\
    \{<[[]]>}<{[{[{[]{()[[[]\n\
    \[<(<(<(<{}))><([]([]()\n\
    \<{([([[(<>()){}]>(<<{{\n\
    \<{([{{}}[<[[[<>{}]]]>[]]\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 26397)]

-- Part Two

scores2 :: [(Char, Int)]
scores2 = [
    (')', 1),
    (']', 2),
    ('}', 3),
    ('>', 4)]

score :: [Char] -> Int
score = foldl add 0
  where
    add n c = n*5 + fromJust (lookup c scores2)

median :: Ord a => [a] -> a
median xs = sort xs !! (length xs `div` 2)

solve2 :: Input -> Int
solve2 ls = median [score bs | Unmatched bs <- map matchBrackets ls]

tests2 :: [(String, Int)]
tests2 = [(testInput, 288957)]

main :: IO ()
main = do
    s <- readFile "input/10.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
