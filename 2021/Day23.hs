module Main where

import Utilities
import Geometry
import Graph
import Data.Char
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map

-- Input processing

-- creature types
data Type = A | B | C | D
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- mapping from type of a destination room to the list of creatures
-- in it, uppermost first
type Rooms = Map Type [Type]

type Input = Rooms

-- x-coordinate of the destination room for the type
destinationX :: Type -> Int
destinationX t = 2*fromEnum t + 3

parse :: String -> Input
parse s =
    Map.fromList [(rt, [t | (x, t) <- xns, x == xn]) |
        rt <- allValues, let xn = destinationX rt]
  where
    xns = [(x, read [c]) | (Position x _, c) <- readGrid s, isUpper c]

-- Part One

-- hallway positions that are not in front of rooms
stopX :: [Int]
stopX = [1..11] \\ map destinationX allValues

-- cost of moving the given type a single step
step_cost :: Type -> Int
step_cost A = 1
step_cost B = 10
step_cost C = 100
step_cost D = 1000

-- type (if any) at each position in the hallway
type Hallway = Map Int Type

data Configuration = Configuration Hallway Rooms
    deriving (Eq, Ord, Show)

finished :: Configuration -> Bool
finished (Configuration hallway rooms) =
    Map.null hallway && and [all (== rt) ts | (rt, ts) <- Map.assocs rooms]

-- there is nothing in the hallway between positions x1 and x2
clear :: Hallway -> Int -> Int -> Bool
clear hallway x1 x2 = and [not (Map.member x hallway) | x <- [lo+1..hi-1]]
  where
    lo = min x1 x2
    hi = max x1 x2

-- the cost of moving t between hallway position x and a room at position rx
-- containing ts (not including the element we are moving)
move_cost :: Int -> Type -> Int -> Int -> [a] -> Int
move_cost room_size t x rx ts =
    step_cost t * (abs (rx-x) + room_size - length ts)

-- moves from a room to the hallway
move_out :: Int -> Configuration -> [(Int, Configuration)]
move_out room_size (Configuration hallway rooms) =
    [(move_cost room_size t x rx ts,
      Configuration (Map.insert x t hallway) (Map.insert rt ts rooms)) |
        (rt, t:ts) <- Map.assocs rooms,
        not (all (== rt) (t:ts)),
        let rx = destinationX rt,
        x <- stopX,
        not (Map.member x hallway),
        clear hallway rx x]

-- moves from the hallway to a room
move_in :: Int -> Configuration -> [(Int, Configuration)]
move_in room_size (Configuration hallway rooms) =
    [(move_cost room_size t x rx (rooms!t),
      Configuration (Map.delete x hallway) (Map.adjust (t:) t rooms)) |
        (x, t) <- Map.assocs hallway,
        all (== t) (rooms!t),
        let rx = destinationX t,
        clear hallway x rx]

-- all possible moves and their costs
moves :: Int -> Configuration -> [(Int, Configuration)]
moves room_size conf = move_out room_size conf ++ move_in room_size conf

solve1 :: Rooms -> Int
solve1 rooms =
    fst $ head $ dropWhile (not . finished . snd) $
        shortestPaths (moves room_size) [Configuration Map.empty rooms]
  where
    room_size = length $ head $ Map.elems rooms

testInput :: String
testInput = "\
    \#############\n\
    \#...........#\n\
    \###B#C#B#D###\n\
    \  #A#D#C#A#\n\
    \  #########\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 12521)]

-- Part Two

unfoldRooms :: Rooms -> Rooms
unfoldRooms rooms = Map.fromList
    [(A, unfoldOne [D, D] (rooms!A)),
     (B, unfoldOne [C, B] (rooms!B)),
     (C, unfoldOne [B, A] (rooms!C)),
     (D, unfoldOne [A, C] (rooms!D))]
  where
    unfoldOne xs (y:ys) = y:xs++ys
    unfoldOne _ [] = error "empty room"

solve2 :: Input -> Int
solve2 = solve1 . unfoldRooms

tests2 :: [(String, Int)]
tests2 = [(testInput, 44169)]

main :: IO ()
main = do
    s <- readFile "input/23.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
