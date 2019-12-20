module Main where

import Utilities
import Geometry
import Intcode
import Data.Map (Map)
import qualified Data.Map as Map

-- Input processing

type Input = Memory

parse :: String -> Input
parse = readMemory

-- Part One

data Direction = U | R | D | L
    deriving (Enum, Show)

showDirection :: Direction -> Char
showDirection U = '^'
showDirection R = '>'
showDirection D = 'v'
showDirection L = '<'

turn :: Int -> Direction -> Direction
turn 0 U = L
turn 0 d = pred d
turn 1 L = U
turn 1 d = succ d
turn n _ = error $ "bad turn instruction " ++ show n

move :: Direction -> Point2 -> Point2
move U (Point2 x y) = Point2 x (y-1)
move R (Point2 x y) = Point2 (x+1) y
move D (Point2 x y) = Point2 x (y+1)
move L (Point2 x y) = Point2 (x-1) y

data Paint = Black | White
    deriving (Enum, Show)

showPaint :: Paint -> Char
showPaint White = '#'
showPaint Black = '.'

-- painting robot

data Robot = Robot {
    direction :: Direction,
    position :: Point2,
    paint :: Map Point2 Paint
    }
    deriving Show

initRobot :: Robot
initRobot = Robot {
    direction = U,
    position = zero2,
    paint = Map.empty
    }

showRobot :: Robot -> String
showRobot r = showGrid ' ' $
    Map.singleton (position r) (showDirection (direction r)) `Map.union`
    fmap showPaint (paint r)

getPaint :: Robot -> Point2 -> Paint
getPaint r p = Map.findWithDefault Black p (paint r)

currPaint :: Robot -> Paint
currPaint r = getPaint r (position r)

paintPanel :: Paint -> Robot -> Robot
paintPanel v r = r { paint = Map.insert (position r) v (paint r) }

turnRobot :: Int -> Robot -> Robot
turnRobot v r = r { direction = turn v (direction r) }

moveRobot :: Robot -> Robot
moveRobot r = r { position = move (direction r) (position r) }

-- running a robot with an Intcode controller

runRobot :: Memory -> Robot -> Robot
runRobot mem r = last rs
  where
    rs = allStates r (mkController mem vs)
    vs = map (fromEnum . currPaint) rs

mkController :: Memory -> [Int] -> [Int]
mkController mem = map fromValue . streamFunction mem . map toValue

-- initial state and all subsequent states from given input
allStates :: Robot -> [Int] -> [Robot]
allStates r0 = scanl step r0 . pairs
  where
    step r (color, dir) =
        moveRobot $ turnRobot dir $ paintPanel (toEnum color) r

-- pair adjacent elements of the list, which must be of even length
pairs :: [a] -> [(a,a)]
pairs (x1:x2:xs) = (x1, x2):pairs xs
pairs [_] = error "unbalanced input"
pairs [] = []

solve1 :: Input -> Int
solve1 mem = Map.size (paint (runRobot mem initRobot))

testInput :: [Int]
testInput = [1,0, 0,0, 1,0, 1,0, 0,1, 1,0, 1,0]

tests1 :: [([Int], Int)]
tests1 = [(testInput, 6)]

-- Part Two

solve2 :: Input -> String
solve2 mem = showRobot $ runRobot mem $ paintPanel White initRobot

main :: IO ()
main = do
    s <- readFile "input/11.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (Map.size . paint . last . allStates initRobot) tests1))
    print (solve1 input)
    putStr (solve2 input)
