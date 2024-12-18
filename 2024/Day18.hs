module Main where

import Graph
import Geometry
import Parser
import Utilities
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = [Position]

parse :: String -> Input
parse = map (runParser position) . lines
  where
    position = Position <$> nat <* char ',' <*> nat

-- Part One

solve1 :: Input -> Int
solve1 = solveAfter 1024

-- length of the shortest path from the start to the finish after n
-- blocks dropped
solveAfter :: Int -> [Position] -> Int
solveAfter n ps =
    length $ takeWhile (\ row -> not (elem finish row)) $
        bfs (moves box blocked) [start]
  where
    box = boundingBox ps
    start = minCorner box
    finish = maxCorner box
    blocked = Set.fromList (take n ps)

-- places in the box one can move to from p avoiding blocked positions
moves :: AABox Position -> Set Position -> Position -> [Position]
moves box blocked p =
    [p' | dp <- unitVectors, let p' = p.+. dp,
        inBox p' box, not (Set.member p' blocked)]

testInput :: String
testInput = "\
    \5,4\n\
    \4,2\n\
    \4,5\n\
    \3,0\n\
    \2,1\n\
    \6,3\n\
    \2,4\n\
    \1,5\n\
    \0,6\n\
    \3,3\n\
    \2,6\n\
    \5,1\n\
    \1,2\n\
    \5,5\n\
    \2,5\n\
    \6,5\n\
    \1,4\n\
    \0,4\n\
    \6,4\n\
    \1,1\n\
    \6,1\n\
    \1,0\n\
    \0,5\n\
    \1,6\n\
    \2,0\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 22)]

-- Part Two

-- the first position that blocks all paths from the start to the finish
solve2 :: Input -> String
solve2 ps =
    showPosition $
        ps!!fromInteger (bsearch (not . solvable . fromInteger) - 1)
  where
    solvable n =
        any (elem finish) $
            bfs (moves box (Set.fromList (take n ps))) [start]
    box = boundingBox ps
    start = minCorner box
    finish = maxCorner box

showPosition :: Position -> String
showPosition (Position x y) = show x ++ "," ++ show y

tests2 :: [(String, String)]
tests2 = [(testInput, "6,1")]

main :: IO ()
main = do
    s <- readFile "input/18.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solveAfter 12 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    putStr (solve2 input)
