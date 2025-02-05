module Main where

import Geometry
import Utilities
import Data.Char
import Data.Map (Map, (!))
import qualified Data.Map as Map

-- Input processing

type Input = [Code]

type Code = [Char]

parse :: String -> Input
parse = lines

-- Directions

data Direction = DRight | DDown | DLeft | DUp
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

readDirection :: Char -> Direction
readDirection '>' = DRight
readDirection 'v' = DDown
readDirection '<' = DLeft
readDirection '^' = DUp
readDirection _ = error "bad direction"

oneStep :: Direction -> Position
oneStep DRight = Position 1 0
oneStep DDown = Position 0 1
oneStep DLeft = Position (-1) 0
oneStep DUp = Position 0 (-1)

-- Commands

data Command = Activate | Move Direction
    deriving (Eq, Ord, Show)

readCommand :: Char -> Command
readCommand 'A' = Activate
readCommand c = Move (readDirection c)

-- Keypads

-- labels at each position
type Keys a = Map Position a
-- keys and the location of the activation key, which is also the start point
data Keypad a = Keypad (Keys a) Position (Map a Position)

readKeypad :: Ord a => (Char -> a) -> String -> Keypad a
readKeypad read_key s = Keypad keys start (invertKeys keys)
  where
    char_keys = Map.fromList [(p, c) | (p, c) <- readGrid s, c /= ' ']
    start = head [p | (p, c) <- Map.assocs char_keys, c == 'A']
    keys = Map.map read_key char_keys

-- The location of the key with each label
invertKeys :: Ord a => Keys a -> Map a Position
invertKeys keys = Map.fromList [(v, pos) | (pos, v) <- Map.assocs keys]

-- There will be a single numeric keypad used at the end.
numericKeypad :: Keypad Char
numericKeypad = readKeypad id "\
    \789\n\
    \456\n\
    \123\n\
    \ 0A\n\
    \"

-- There will be many copied of the directional keypad, each controlling
-- a robot.
directionalKeypad :: Keypad Command
directionalKeypad = readKeypad readCommand "\
    \ ^A\n\
    \<v>\n\
    \"

-- Part One

solve1 :: Input -> Int
solve1 = genSolve 2

testInput :: String
testInput = "\
    \029A\n\
    \980A\n\
    \179A\n\
    \456A\n\
    \379A\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 126384)]

-- Part Two

-- For long chains of indirection, we need to memoize the costs for
-- shorter chains.  We will do this starting with the human controller
-- and moving through the robots controlled by directional keypads until
-- we reach the numeric keypad.

-- Cost of pressing a key at p2 if the previously pushed one was at p1.
-- The otherwise-unused type parameter indicates the type of the labels
-- on the keys the positions refer to, which vary between keypads.
type Costs a = Map (Position, Position) Int

-- Base case: when we are operating a keypad directly, moving is free
-- and we make only the required keypress.
baseCosts :: Keypad a -> Costs a
baseCosts (Keypad keys _ _) =
    Map.fromList [((p1, p2), 1) | p1 <- Map.keys keys, p2 <- Map.keys keys]

-- For a given keypad (assumed convex), the cost of moving the next robot
-- from p1 to p2 and pressing the key there, given the costs of moving
-- between positions on the directional keypad controlling this robot.
--
-- A path from p1 to p2 will be expressed as a sequence of directional
-- commands.  To send these commands to the next robot, the controller
-- starts from the Activate key, moves to each of the required direction
-- keys in turn, pressing them, and then moves back to the Activate key
-- and presses that.  There may be more than one such path: we choose the
-- one that minimizes the costs of the above moves for the controller.
--
-- This version does some repeated work, computing the paths each time
-- and not using the fact that parallel paths have identical costs.
-- However, doing it this way is simpler, not much slower and handles
-- the numeric keypad as well.
stepCosts :: Keypad a -> Costs Direction -> Costs a
stepCosts (Keypad keys _ _) costs =
    Map.fromListWith min
        [((p1, p2),
          costWalk directionalKeypad costs (map Move ds)) |
            p1 <- Map.keys keys,
            p2 <- Map.keys keys,
            ds <- directedWalk keys p1 p2]

-- All the ways of walking from p1 to p2 on the keypad (assumed convex)
-- without going backwards.
directedWalk :: Keys a -> Position -> Position -> [[Direction]]
directedWalk keys p1 p2
  | p1 == p2 = [[]]
  | otherwise =
    [d:ds |
        d <- allValues,
        let p1' = p1 .+. oneStep d,
        Map.member p1' keys,
        distance p1' p2 < distance p1 p2,
        ds <- directedWalk keys p1' p2]

-- The cost of moving through the keypad from the start through all
-- the positions of the keys with the given labels and back to the
-- start again.
costWalk :: Ord a => Keypad a -> Costs a -> [a] -> Int
costWalk (Keypad _ start positions) costs labels =
    sum $ map (costs!) $ steps start $ map (positions!) labels

-- Steps from the start through all the xs and back to the start.
-- steps start [x1,...,xn] = [(start, x1), (x1, x2), ... (xn, start)]
steps :: a -> [a] -> [(a,a)]
steps start xs = zip (start : xs) (xs ++ [start])

-- Type each code on the numeric keypad via a chain of n robots each
-- controlled by a directional keypad.
-- We assume that the last letter of each code is 'A'.
genSolve :: Int -> Input -> Int
genSolve n codes =
    sum [complexity code (costWalk numericKeypad costs (init code)) |
        code <- codes]
  where
    costs =
        stepCosts numericKeypad $
        times n (stepCosts directionalKeypad) $
        baseCosts directionalKeypad

complexity :: Code -> Int -> Int
complexity code len = len * read (filter isDigit code)

solve2 :: Input -> Int
solve2 = genSolve 25

main :: IO ()
main = do
    s <- readFile "input/21.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    print (solve2 input)
