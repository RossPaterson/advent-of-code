module Main where

import Geometry
import Parser
import Utilities

-- Input processing

data Direction = R | D | L | U
    deriving (Bounded, Enum, Eq, Ord, Read, Show)
data Dig = Dig Direction Int Colour
    deriving (Show)
type Colour = Int

type Input = [Dig]

parse :: String -> Input
parse = map (runParser dig) . lines
  where
    dig = Dig <$> direction <* space <*> nat <* space <*> colour
    direction = enumValue
    colour = string "(#" *> hexNumber <* char ')'

-- Part One

-- direction and distance from a digging instruction
trench :: Dig -> (Direction, Int)
trench (Dig dir n _) = (dir, n)

-- one step in the specified direction
oneStep :: Direction -> Point2
oneStep U = Point2 0 1
oneStep R = Point2 1 0
oneStep D = Point2 0 (-1)
oneStep L = Point2 (-1) 0

-- specified number of steps in the given direction
steps :: (Direction, Int) -> Point2
steps (dir, n) = n *. oneStep dir

-- area of the loop (one cell wide) and its interior,
-- using a simple case of Pick's theorem
totalArea :: [(Direction, Int)] -> Int
totalArea instrs = (polygonArea2 loop + perimeter) `div` 2 + 1
  where
    loop = scanl (.+.) zero (map steps instrs)
    perimeter = sum (map snd instrs)

solve1 :: Input -> Int
solve1 = totalArea . map trench

testInput :: String
testInput = "\
    \R 6 (#70c710)\n\
    \D 5 (#0dc571)\n\
    \L 2 (#5713f0)\n\
    \D 2 (#d2c081)\n\
    \R 2 (#59c680)\n\
    \D 2 (#411b91)\n\
    \L 5 (#8ceee2)\n\
    \U 2 (#caa173)\n\
    \L 1 (#1b58a2)\n\
    \U 2 (#caa171)\n\
    \R 2 (#7807d2)\n\
    \U 3 (#a77fa3)\n\
    \L 2 (#015232)\n\
    \U 2 (#7a21e3)\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 62)]

-- Part Two

-- revised direction and distance from a digging instruction
trench2 :: Dig -> (Direction, Int)
trench2 (Dig _ _ n) = (toEnum (n `mod` 16), n `div` 16)

solve2 :: Input -> Int
solve2 = totalArea . map trench2

tests2 :: [(String, Int)]
tests2 = [(testInput, 952408144115)]

main :: IO ()
main = do
    s <- readFile "input/18.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
