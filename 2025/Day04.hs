module Main where

import Geometry
import Utilities
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = Set Position

parse :: String -> Input
parse s = Set.fromList [p | (p, c) <- readGrid s, c == '@']

-- Part One

solve1 :: Input -> Int
solve1 ps = Set.size $ Set.filter (accessible ps) ps

accessible :: Set Position -> Position -> Bool
accessible ps p = Set.size (Set.intersection ps (neighbours p)) < 4

neighbours :: Position -> Set Position
neighbours p = Set.fromList [p .+. d | d <- corners ++ unitVectors]

testInput :: String
testInput = "\
    \..@@.@@@@.\n\
    \@@@.@.@.@@\n\
    \@@@@@.@.@@\n\
    \@.@@@@..@.\n\
    \@@.@@@@.@@\n\
    \.@@@@@@@.@\n\
    \.@.@.@.@@@\n\
    \@.@@@.@@@@\n\
    \.@@@@@@@@.\n\
    \@.@.@@@.@.\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 13)]

-- Part Two

solve2 :: Input -> Int
solve2 ps = Set.size ps - Set.size (whileJust remove ps)

-- remove the accessible elements, if any
remove :: Set Position -> Maybe (Set Position)
remove ps
  | Set.null removable = Nothing
  | otherwise = Just (Set.difference ps removable)
  where
    removable = Set.filter (accessible ps) ps

tests2 :: [(String, Int)]
tests2 = [(testInput, 43)]

main :: IO ()
main = do
    s <- readFile "input/04.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
