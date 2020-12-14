module Main where

import Geometry
import Parser
import Utilities
import Control.Applicative

-- Input processing

type Input = [(Action, Int)]

data Direction = N | E | S | W
    deriving (Bounded, Enum, Show)

data Action = Move Direction | TurnLeft | TurnRight | Forward
    deriving (Show)

parse :: String -> Input
parse = map (runParser cmd) . lines
  where
    cmd = (,) <$> action <*> nat
    action =
        Move <$> enumValue <|>
        TurnLeft <$ char 'L' <|>
        TurnRight <$ char 'R' <|>
        Forward <$ char 'F' 

-- Part One

-- one step in the given direction
direction :: Direction -> Point2
direction N = Point2 0 1
direction S = Point2 0 (-1)
direction E = Point2 1 0
direction W = Point2 (-1) 0

data State = State { position :: Point2, waypoint :: Point2 }
    deriving (Show)

initState1 :: State
initState1 = State zero (direction E)

move :: Direction -> Int -> State -> State
move dir n s = s { position = position s .+. n *. direction dir }

-- rotate the point clockwise by 90, 180 or 270 degrees around the origin
rotate :: Int -> Point2 -> Point2
rotate angle (Point2 x y)
  | angle == 90 = Point2 y (-x)
  | angle == 180 = Point2 (-x) (-y)
  | angle == 270 = Point2 (-y) x
  | otherwise = error $ "bad angle " ++ show angle

-- rotate waypoint clockwise (right)
turn :: Int -> State -> State
turn angle s = s { waypoint = rotate angle (waypoint s) }

forward :: Int -> State -> State
forward n s = s { position = position s .+. n *. waypoint s }

action1 :: State -> (Action, Int) -> State
action1 s (Move dir, n) = move dir n s
action1 s (TurnLeft, n) = turn (360-n) s
action1 s (TurnRight, n) = turn n s
action1 s (Forward, n) = forward n s

solve1 :: Input -> Int
solve1 = norm . position . foldl action1 initState1

testInput :: String
testInput = "\
    \F10\n\
    \N3\n\
    \F7\n\
    \R90\n\
    \F11\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 25)]

-- Part Two

initState2 :: State
initState2 = State zero (Point2 10 1)

move_waypoint :: Direction -> Int -> State -> State
move_waypoint dir n s = s { waypoint = waypoint s .+. n *. direction dir }

action2 :: State -> (Action, Int) -> State
action2 s (Move dir, n) = move_waypoint dir n s
action2 s (TurnLeft, n) = turn (360-n) s
action2 s (TurnRight, n) = turn n s
action2 s (Forward, n) = forward n s

solve2 :: Input -> Int
solve2 = norm . position . foldl action2 initState2

tests2 :: [(String, Int)]
tests2 = [(testInput, 286)]

main :: IO ()
main = do
    s <- readFile "input/12.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
