module Main where

import Utilities
import Cartesian
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

-- the set of seat positions
type Input = Set Position

parse :: String -> Input
parse s = Set.fromList [p | (p, c) <- readGrid s, c == 'L']

-- Part One

-- unit vectors for the eight directions
directions :: [Position]
directions = cardinalDirections ++ corners

-- for each seat, the set of seats that are immediately adjacent to it
adjacentSeats :: Set Position -> Map Position (Set Position)
adjacentSeats seats = Map.fromSet adjacent seats
  where
    adjacent p = Set.intersection seats (Set.fromList (map (p .+.) directions))

-- cellular automaton rule: given whether this cell is live and the number
-- of live neighbours, will it be live on the next iteration?
type CellRule = Bool -> Int -> Bool

-- seating rule for part one
rule1 :: CellRule
rule1 False n = n == 0
rule1 True n = n < 4

-- Update the set of occupied seats according to the given rule and
-- map yielding the set of seats affecting each seat.
step :: Ord a => CellRule -> Map a (Set a) -> Set a -> Set a
step rule seats occupied = Map.keysSet $ Map.filterWithKey sit seats
  where
    sit p neighbours =
        rule (Set.member p occupied)
            (Set.size (Set.intersection neighbours occupied))

-- seats occupied when the system reaches a fixed point
finalOccupied :: Ord a => CellRule -> Map a (Set a) -> Set a
finalOccupied rule seats =
    convergeBy (==) $ iterate (step rule seats) Set.empty

solve1 :: Input -> Int
solve1 = Set.size . finalOccupied rule1 . adjacentSeats

testInput :: String
testInput = "\
    \L.LL.LL.LL\n\
    \LLLLLLL.LL\n\
    \L.L.L..L..\n\
    \LLLL.LL.LL\n\
    \L.LL.LL.LL\n\
    \L.LLLLL.LL\n\
    \..L.L.....\n\
    \LLLLLLLLLL\n\
    \L.LLLLLL.L\n\
    \L.LLLLL.LL\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 37)]

-- Part Two

-- for each seat, the set of seats that are visible in the 8 directions
seatsVisible :: Set Position -> Map Position (Set Position)
seatsVisible seats = Map.fromSet visible seats
  where
    visible p = Set.fromList [p' | d <- directions,
        p' <- take 1 $ filter (flip Set.member seats) $ map (p .+.) $ ray d]
    ray d = [n *. d | n <- [1..maxdist]]
    maxdist = maximum [max x y | Position x y <- Set.elems seats]

-- seating rule for part two
rule2 :: CellRule
rule2 False n = n == 0
rule2 True n = n < 5

solve2 :: Input -> Int
solve2 = Set.size . finalOccupied rule2 . seatsVisible

tests2 :: [(String, Int)]
tests2 = [(testInput, 26)]

main :: IO ()
main = do
    s <- readFile "input/11.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
