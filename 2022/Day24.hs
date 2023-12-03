module Main where

import Utilities
import Geometry
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = (Valley, Blizzards)

data Valley = Valley { width :: Int, height :: Int }
    deriving (Show)

-- set of blizzards moving in each direction
type Blizzards = Map Direction (Set Position)

data Direction = DRight | DDown | DLeft | DUp
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

dirName :: Direction -> Char
dirName DRight = '>'
dirName DDown = 'v'
dirName DLeft = '<'
dirName DUp = '^'

parse :: String -> Input
parse s =  (v, bs)
  where
    pcs = readGrid s
    v = Valley {
        width = maximum [x | (Position x _, _) <- pcs] - 1,
        height = maximum [y | (Position _ y, _) <- pcs] - 1
        }
    bs =
        Map.fromListWith Set.union [(dir, Set.singleton p) |
            (p, c) <- pcs, c /= '.' && c /= '#',
            dir <- allValues, dirName dir == c]

-- Part One

-- Layout of the valley

start :: Position
start = Position 1 0

goal :: Valley -> Position
goal v = Position (width v) (height v + 1)

-- Moves possible within the valley (ignoring blizzards)
moves :: Valley -> Position -> [Position]
moves v p = p:filter (inValley v) (map (p .+.) unitVectors)

inValley :: Valley -> Position -> Bool
inValley v p = inBox p (valleyBox v) && not (inWall v p)

valleyBox :: Valley -> AABox Position
valleyBox v = singletonBox zero <> singletonBox (Position (w+1) (h+1))
  where
    w = width v
    h = height v

inWall :: Valley -> Position -> Bool
inWall v (Position x y) =
    x == 0 || x == w+1 || y == 0 && x > 1 || y == h+1 && x < w
  where
    w = width v
    h = height v

-- Key idea: simulate all expeditions in parallel.
-- The state records all possible locations of the expedition on a
-- particular iteration.
data State = State {
    locations :: Set Position,
    blizzards :: Blizzards
    }
    deriving (Show)

startState :: Position -> Blizzards -> State
startState p0 bs = State { locations = Set.singleton p0, blizzards = bs }

-- Display the state for debugging purposes
showState' :: Valley -> State -> String
showState' v s = showBox (valleyBox v) showPos
  where
    showPos p
      | inWall v p = '#'
      | Set.member p (locations s) = 'E'
      | otherwise = '.'

showState :: Valley -> State -> String
showState v s = showGrid '.' $
    Map.unions [
        blizzardMap (blizzards s),
        Map.fromSet (const 'E') (locations s),
        Map.fromList [(p, '#') | p <- walls]]
  where
    w = width v
    h = height v
    walls =
        [Position 0 y | y <- [0..h]] ++
        [Position x 0 | x <- [2..w]] ++
        [Position (w+1) y | y <- [0..h]] ++
        [Position x (h+1) | x <- [1..w-1]]

blizzardMap :: Blizzards -> Map Position Char
blizzardMap bs =
    Map.map render $ Map.fromListWith (++) [(p, [c]) |
        (dir, ps) <- Map.assocs bs, let c = dirName dir, p <- Set.elems ps]
  where
    render [c] = c
    render cs = head (show (length cs))

-- Blizzard movement
moveBlizzards :: Valley -> Blizzards -> Blizzards
moveBlizzards v = Map.mapWithKey (moveBlizzard v)

moveBlizzard :: Valley -> Direction -> Set Position -> Set Position
moveBlizzard v dir = Set.map (movePosition v dir)

movePosition :: Valley -> Direction -> Position -> Position
movePosition v DRight (Position x y)
  | x == width v = Position 1 y
  | otherwise = Position (x+1) y
movePosition v DDown (Position x y)
  | y == height v = Position x 1
  | otherwise = Position x (y+1)
movePosition v DLeft (Position x y)
  | x == 1 = Position (width v) y
  | otherwise = Position (x-1) y
movePosition v DUp (Position x y)
  | y == 1 = Position x (height v)
  | otherwise = Position x (y-1)

-- One step in the parallel simulation of all positions of the expedition
step :: Valley -> State -> State
step v s = State {
    locations = Set.difference locs' blizzard_locs,
    blizzards = bs'
    }
  where
    bs' = moveBlizzards v (blizzards s)
    blizzard_locs = Set.unions (Map.elems bs')
    locs' =
        Set.fromList $ [p' |
            p <- Set.elems (locations s), p' <- moves v p]

-- List of states from the start (not included) to the goal (included)
travel :: Valley -> Blizzards -> Position -> Position -> [State]
travel v bs p_start p_goal = tail front ++ [final]
  where
    finished s = Set.member p_goal (locations s)
    (front, final:_) =
        span (not . finished) $
        iterate (step v) $
        startState p_start bs

solve1 :: Input -> Int
solve1 (v, bs) = length (travel v bs start (goal v))

smallInput :: String
smallInput = "\
    \#.#####\n\
    \#.....#\n\
    \#>....#\n\
    \#.....#\n\
    \#...v.#\n\
    \#.....#\n\
    \#####.#\n"

testInput :: String
testInput = "\
    \#.######\n\
    \#>>.<^<#\n\
    \#.<..<<#\n\
    \#>v.><>#\n\
    \#<^v^^>#\n\
    \######.#\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 18)]

-- Part Two

solve2 :: Input -> Int
solve2 (v, bs) = length journey1 + length journey2 + length journey3
  where
    journey1 = travel v bs start (goal v)
    journey2 = travel v (blizzards (last journey1)) (goal v) start
    journey3 = travel v (blizzards (last journey2)) start (goal v)

tests2 :: [(String, Int)]
tests2 = [(testInput, 54)]

main :: IO ()
main = do
    s <- readFile "input/24.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
