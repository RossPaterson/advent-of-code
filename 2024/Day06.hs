module Main where

import Geometry
import Utilities
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = (AABox Position, Set Position, State)

data State = State {
    loc :: !Position,
    dir :: !Position
    }
    deriving (Eq, Ord, Show)

parse :: String -> Input
parse s =
    (boundingBox (map fst pcs),
     Set.fromList [p | (p, c) <- pcs, c == '#'],
     State { loc = guard_loc, dir = direction dir_c })
  where
    pcs = readGrid s
    (guard_loc, dir_c) = head [(p, c) | (p, c) <- pcs, c /= '.' && c /= '#']

direction :: Char -> Position
direction 'v' = Position 0 1
direction '^' = Position 0 (-1)
direction '>' = Position 1 0
direction '<' = Position (-1) 0
direction _ = error "bad guard"

-- Part One

solve1 :: Input -> Int
solve1 (area, obstructions, start) =
    Set.size (visited area obstructions start)

visited :: AABox Position -> Set Position -> State -> Set Position
visited area obstructions =
    Set.fromList . map loc . path area obstructions

-- path taken by the guard until they leave the area
path :: AABox Position -> Set Position -> State -> [State]
path area obstructions =
    takeWhile (\ s -> inBox (loc s) area) .
    iterate (step obstructions)

-- step forward if possible, otherwise turn right
step :: Set Position -> State -> State
step blocks s
  | Set.member next_p blocks = s { dir = rotateSectors 3 (dir s) }
  | otherwise = s { loc = next_p }
  where
    next_p = loc s .+. dir s

testInput :: String
testInput = "\
    \....#.....\n\
    \.........#\n\
    \..........\n\
    \..#.......\n\
    \.......#..\n\
    \..........\n\
    \.#..^.....\n\
    \........#.\n\
    \#.........\n\
    \......#...\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 41)]

-- Part Two

-- The number of ways we can create a cycle by placing one block.
-- We need only consider positions along the original path, as others
-- will have no effect.
solve2 :: Input -> Int
solve2 (area, obstructions, start) =
    length [p |
        p <- tail (Set.elems (visited area obstructions start)),
        hasRepeat (path area (Set.insert p obstructions) start)]

-- the list has a repeated element
hasRepeat :: Ord a => [a] -> Bool
hasRepeat xs = or (zipWith Set.member xs (initSets xs))

tests2 :: [(String, Int)]
tests2 = [(testInput, 6)]

main :: IO ()
main = do
    s <- readFile "input/06.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
