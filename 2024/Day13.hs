module Main where

import Parser
import Utilities
import Data.Maybe
import Data.Ratio ((%))

-- Input processing

type Input = [Machine]

data Machine = Machine Button Button Prize
    deriving Show

type Button = (Integer, Integer)
type Prize = (Integer, Integer)

parse :: String -> Input
parse = map (runParser machine) . paragraphs
  where
    machine = Machine <$> button <*> button <*> prize
    button = (,) <$ string "Button " <* letter <* string ": X+" <*> nat <* string ", Y+" <*> nat <* char '\n'
    prize = (,) <$ string "Prize: X=" <*> nat <* string ", Y=" <*> nat <* char '\n'

-- Part One

type Plan = (Integer, Integer)

solve1 :: Input -> Integer
solve1 = sum . map cost . mapMaybe integralSolution

cost :: Plan -> Integer
cost (a, b) = 3*a + b

-- Find the integral solution to the pair of equations, if any
integralSolution :: Machine -> Maybe Plan
integralSolution (Machine (xa, ya) (xb, yb) (x, y))
  | d == 0 = Nothing
  | otherwise = (,) <$> asInteger a <*> asInteger b
  where
    d = xb*ya - xa*yb
    b = (x*ya - xa*y) % d
    a = (xb*y - x*yb) % d

-- Convert to Integer, if possible
asInteger :: Rational -> Maybe Integer
asInteger x
  | fromIntegral floor_x == x = Just floor_x
  | otherwise = Nothing
  where
    floor_x = floor x

testInput :: String
testInput = "\
    \Button A: X+94, Y+34\n\
    \Button B: X+22, Y+67\n\
    \Prize: X=8400, Y=5400\n\
    \\n\
    \Button A: X+26, Y+66\n\
    \Button B: X+67, Y+21\n\
    \Prize: X=12748, Y=12176\n\
    \\n\
    \Button A: X+17, Y+86\n\
    \Button B: X+84, Y+37\n\
    \Prize: X=7870, Y=6450\n\
    \\n\
    \Button A: X+69, Y+23\n\
    \Button B: X+27, Y+71\n\
    \Prize: X=18641, Y=10279\n\
    \"

tests1 :: [(String, Integer)]
tests1 = [(testInput, 480)]

-- Part Two

solve2 :: Input -> Integer
solve2 = solve1 . map correct

correct :: Machine -> Machine
correct (Machine a b (x, y)) =
    Machine a b (x+10000000000000, y+10000000000000)

main :: IO ()
main = do
    s <- readFile "input/13.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    print (solve2 input)
