module Main where

import Utilities
import qualified Data.Map as Map
import Data.Map (Map)

type Input = [Int]

parse :: String -> Input
parse = map read . words

data State = State Int (Map Int Int)

mkState :: Input -> State
mkState ns = State 0 (Map.fromList (zip [0..] ns))

step1 :: State -> Maybe State
step1 (State pos m) = do
    offset <- Map.lookup pos m
    let offset' = offset+1
    return (State (pos + offset) (Map.insert pos offset' m))

solve1 :: Input -> Int
solve1 = subtract 1 . length . iterateWhileJust step1 . mkState

tests1 :: [(String, Int)]
tests1 = [("0 3  0  1  -3", 5)]

-- Part Two

step2 :: State -> Maybe State
step2 (State pos m) = do
    offset <- Map.lookup pos m
    let offset' = if offset >= 3 then offset-1 else offset+1
    return (State (pos + offset) (Map.insert pos offset' m))

solve2 :: Input -> Int
solve2 = subtract 1 . length . iterateWhileJust step2 . mkState

tests2 :: [(String, Int)]
tests2 = [("0 3  0  1  -3", 10)]

main :: IO ()
main = do
    s <- readFile "input05.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
