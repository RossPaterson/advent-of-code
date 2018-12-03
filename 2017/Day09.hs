module Main where

import Parser
import Utilities
import Control.Applicative

data Tree = Garbage Int | Group [Tree]
    deriving Show

type Input = Tree

parse :: String -> Input
parse = runParser tree . head . lines
  where
    tree =
        Garbage <$ char '<' <*> (sum <$> many junk) <* char '>' <|>
        Group <$ char '{' <*> trees <* char '}'
    junk =
        pure 0 <* char '!' <* anyChar <|>
        pure 1 <* satisfy (\ c -> c /= '!' && c /= '>')
    trees = sepBy1 tree (char ',') <|> pure []

sumDepth :: Tree -> Int
sumDepth = sd 1
  where
    sd n (Garbage _) = 0
    sd n (Group ts) = n + sum [sd (n+1) t | t <- ts]

solve1 :: Input -> Int
solve1 = sumDepth

tests1 :: [(String, Int)]
tests1 = [
    ("{}", 1),
    ("{{{}}}", 6),
    ("{{},{}}", 5),
    ("{{{},{},{{}}}}", 16),
    ("{<a>,<a>,<a>,<a>}", 1),
    ("{{<!!>},{<!!>},{<!!>},{<!!>}}", 9),
    ("{{<!!>},{<!!>},{<!!>},{<!!>}}", 9),
    ("{{<a!>},{<a!>},{<a!>},{<ab>}}", 3)]

-- Part Two

garbage :: Tree -> Int
garbage (Garbage n) = n
garbage (Group ts) = sum (map garbage ts)

solve2 :: Input -> Int
solve2 = garbage

tests2 :: [(String, Int)]
tests2 = [
    ("<>", 0),
    ("<random characters>", 17),
    ("<<<<>", 3),
    ("<{!>}>", 2),
    ("<!!>", 0),
    ("<!!!>>", 0),
    ("<{o\"i!a,<{i<a>", 10)]

main :: IO ()
main = do
    s <- readFile "input09.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
