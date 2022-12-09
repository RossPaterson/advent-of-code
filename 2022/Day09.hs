module Main where

import Utilities
import Geometry
import Parser
import qualified Data.Set as Set

-- Input processing

type Input = [Move]
data Move = Move Direction Int
    deriving (Show)
data Direction = R | U | L | D -- counterclockwise order
    deriving (Bounded, Enum, Read, Show)

parse :: String -> Input
parse s = map (runParser move) (lines s)
  where
    move = Move <$> direction <* space <*> nat
    direction = enumValue

-- Part One

expandMove :: Move -> [Direction]
expandMove (Move d n) = replicate n d

moveHead :: Point2 -> Direction -> Point2
moveHead p d = p .+. unitVector (fromEnum d)

-- sequence of positions the head moves to
headPositions :: [Move] -> [Point2]
headPositions = tail . scanl moveHead zero . concatMap expandMove

-- drag the tail position towards the head position
dragTail :: Point2 -> Point2 -> Point2
dragTail tp hp = hp .+. makeClose (tp .-. hp)

-- at most 1 in each direction of the original
makeClose :: Point2 -> Point2
makeClose (Point2 x y) = case compare (abs x) (abs y) of
    LT -> Point2 0 (signum y)
    EQ -> Point2 (signum x) (signum y)
    GT -> Point2 (signum x) 0

solve1 :: Input -> Int
solve1 = Set.size . Set.fromList . scanl dragTail zero . headPositions

testInput :: String
testInput = "\
    \R 4\n\
    \U 4\n\
    \L 3\n\
    \D 1\n\
    \R 4\n\
    \D 1\n\
    \L 5\n\
    \R 2\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 13)]

-- Part Two

dragTails :: [Point2] -> Point2 -> [Point2]
dragTails [] _ = []
dragTails (tp:tps) hp = tp':dragTails tps tp'
  where
    tp' = dragTail tp hp

solve2 :: Input -> Int
solve2 =
    Set.size . Set.fromList . map last .
        scanl dragTails (replicate 9 zero) . headPositions

testInput2 :: String
testInput2 = "\
    \R 5\n\
    \U 8\n\
    \L 8\n\
    \D 3\n\
    \R 17\n\
    \D 10\n\
    \L 25\n\
    \U 20\n"

tests2 :: [(String, Int)]
tests2 = [(testInput, 1), (testInput2, 36)]

main :: IO ()
main = do
    s <- readFile "input/09.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
