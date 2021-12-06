module Main where

import Utilities
import Data.Map (Map)
import qualified Data.Map as Map

-- Input processing

type Input = [Int]

parse :: String -> Input
parse = readNumbers

-- Part One

-- number of fish with internal times of 0..8
-- (all fish with the same counter are identical)
data State = State
    !Integer !Integer !Integer !Integer !Integer
    !Integer !Integer !Integer !Integer
    deriving Show

initState :: [Int] -> State
initState = entries . Map.fromList . frequency

entries :: Map Int Int -> State
entries m = State
    (toInteger (Map.findWithDefault 0 0 m))
    (toInteger (Map.findWithDefault 0 1 m))
    (toInteger (Map.findWithDefault 0 2 m))
    (toInteger (Map.findWithDefault 0 3 m))
    (toInteger (Map.findWithDefault 0 4 m))
    (toInteger (Map.findWithDefault 0 5 m))
    (toInteger (Map.findWithDefault 0 6 m))
    (toInteger (Map.findWithDefault 0 7 m))
    (toInteger (Map.findWithDefault 0 8 m))

step :: State -> State
step (State n0 n1 n2 n3 n4 n5 n6 n7 n8) =
    State n1 n2 n3 n4 n5 n6 (n7+n0) n8 n0

total :: State -> Integer
total (State n0 n1 n2 n3 n4 n5 n6 n7 n8) = n0+n1+n2+n3+n4+n5+n6+n7+n8

solve1 :: Input -> Integer
solve1 = total . times 80 step . initState

testInput :: String
testInput = "3,4,3,1,2"

tests1 :: [(String, Integer)]
tests1 = [(testInput, 5934)]

-- Part Two

solve2 :: Input -> Integer
solve2 = total . times 256 step . initState

tests2 :: [(String, Integer)]
tests2 = [(testInput, 26984457539)]

main :: IO ()
main = do
    s <- readFile "input/06.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
