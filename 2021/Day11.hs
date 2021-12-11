module Main where

import Utilities
import Data.Char
import Data.List
import Geometry
import Data.Map (Map)
import qualified Data.Map as Map

-- Input processing

type Grid = Map Position Int
type Input = Grid

parse :: String -> Input
parse = fmap digitToInt . Map.fromList . readGrid

-- Part One

neighbours :: Position -> [Position]
neighbours p = [p .+. d | d <- unitVectors ++ corners]

-- if any have flashed, remove them and increment their neighbours
removeFlashed :: Grid -> Maybe Grid
removeFlashed g
  | Map.null flashes = Nothing
  | otherwise = Just (Map.difference (Map.unionWith (+) increments g) flashes)
  where
    flashes = Map.filter (> 9) g
    increments =
        Map.intersection
            (Map.fromListWith (+)
                [(p', 1) | p <- Map.keys flashes, p' <- neighbours p])
            g

-- update the grid with a flash step, noting how many flashed
flashStep :: Grid -> (Int, Grid)
flashStep g = (Map.size g - Map.size g', Map.union g' (fmap (const 0) g))
  where
    g' = whileJust removeFlashed (fmap (+1) g)

flashCounts :: Grid -> [Int]
flashCounts = unfoldr (Just . flashStep)

solve1 :: Input -> Int
solve1 = sum . take 100 . flashCounts

testInput :: String
testInput = "\
    \5483143223\n\
    \2745854711\n\
    \5264556173\n\
    \6141336146\n\
    \6357385478\n\
    \4167524645\n\
    \2176841721\n\
    \6882881134\n\
    \4846848554\n\
    \5283751526\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 1656)]

-- Part Two

solve2 :: Input -> Int
solve2 g = 1 + length (takeWhile (< Map.size g) (flashCounts g))

tests2 :: [(String, Int)]
tests2 = [(testInput, 195)]

main :: IO ()
main = do
    s <- readFile "input/11.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
