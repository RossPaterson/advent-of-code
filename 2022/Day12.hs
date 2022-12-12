module Main where

import Utilities
import Geometry
import Graph
import Data.Char
import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as Map

-- Input processing

type Input = HeightMap
type HeightMap = (Map Position Int, Position, Position)

parse :: String -> Input
parse s = (height_map, start, end)
  where
    height_map = Map.fromList [(p, height c) | (p, c) <- grid]
    start = head [p | (p, c) <- grid, c == 'S']
    end = head [p | (p, c) <- grid, c == 'E']
    grid = readGrid s
    height 'S' = 0
    height 'E' = 25
    height c = ord c - ord 'a'

-- Part One

neighbours :: Map Position Int -> Position -> [Position]
neighbours m p =
    [p' | u <- unitVectors, let p' = p .+. u,
        h' <- maybeToList (Map.lookup p' m), h' <= h+1]
  where
    h = m ! p

pathLength :: Map Position Int -> Position -> Position -> Maybe Int
pathLength m s e
  | null back = Nothing
  | otherwise = Just (length front)
  where
    (front, back) = span (notElem e) $ bfs (neighbours m) [s]

solve1 :: Input -> Int
solve1 (m, s, e) = fromMaybe (error "no path") (pathLength m s e)

testInput :: String
testInput = "\
    \Sabqponm\n\
    \abcryxxl\n\
    \accszExk\n\
    \acctuvwj\n\
    \abdefghi\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 31)]

-- Part Two

solve2 :: Input -> Int
solve2 (m, _, e) =
    minimum [n | (p, h) <- Map.assocs m, h == 0,
        n <- maybeToList (pathLength m p e)]

tests2 :: [(String, Int)]
tests2 = [(testInput, 29)]

main :: IO ()
main = do
    s <- readFile "input/12.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
