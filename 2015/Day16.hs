module Main where

import Parser
import Control.Applicative
import Data.Map (Map)
import qualified Data.Map as Map

type Possessions = Map String Int
type Entry = (Int, Possessions)
type Input = [Entry]

parse :: String -> Input
parse = map (runParser entry) . lines
  where
    entry = (,) <$ string "Sue " <*> nat <* string ": " <*> possessions
    possessions = Map.fromList <$> (possession `sepBy1` string ", ")
    possession = (,) <$> name <* string ": " <*> nat
    name = some letter

match :: Possessions -> Possessions -> Bool
match full partial =
    and $ Map.elems $ Map.intersectionWith (==) full partial

results :: Possessions
results = Map.fromList [
    ("children", 3),
    ("cats", 7),
    ("samoyeds", 2),
    ("pomeranians", 3),
    ("akitas", 0),
    ("vizslas", 0),
    ("goldfish", 5),
    ("trees", 3),
    ("cars", 2),
    ("perfumes", 1)]

solve1 :: Input -> Int
solve1 es = head [n | (n, ps) <- es, match results ps]

-- Part Two --

type Output2 = Map String (Int -> Bool)

results2 :: Output2
results2 = Map.fromList [
    ("children", (== 3)),
    ("cats", (> 7)),
    ("samoyeds", (== 2)),
    ("pomeranians", (< 3)),
    ("akitas", (== 0)),
    ("vizslas", (== 0)),
    ("goldfish", (< 5)),
    ("trees", (> 3)),
    ("cars", (== 2)),
    ("perfumes", (== 1))]

match2 :: Output2 -> Possessions -> Bool
match2 output partial =
    and $ Map.elems $ Map.intersectionWith ($) output partial

solve2 :: Input -> Int
solve2 es = head [n | (n, ps) <- es, match2 results2 ps]

main :: IO ()
main = do
    s <- readFile "input/16.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
