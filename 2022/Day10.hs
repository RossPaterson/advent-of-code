module Main where

import Utilities
import Parser
import Control.Applicative
import Data.List

-- Input processing

type Input = [Instruction]
data Instruction
    = AddX Int
    | NoOp
    deriving (Show)

parse :: String -> Input
parse s = map (runParser instruction) (lines s)
  where
    instruction =
        AddX <$ string "addx " <*> int <|>
        NoOp <$ string "noop"

-- Part One

-- cycle number and the value of x in that cycle
type State = (Int, Int)

action :: Int -> Instruction -> (Int, [Int])
action x (AddX n) = (x+n, [x, x])
action x NoOp = (x, [x])

values :: [Instruction] -> [State]
values instrs = zip [1..] (concat (snd (mapAccumL action 1 instrs)))

sample :: [State] -> Int
sample vs = sum [clock * v | (clock, v) <- vs, clock `mod` 40 == 20]

solve1 :: Input -> Int
solve1 = sample . values

smallInput :: String
smallInput = "\
    \noop\n\
    \addx 3\n\
    \addx -5\n"

testInput :: String
testInput = "\
    \addx 15\n\
    \addx -11\n\
    \addx 6\n\
    \addx -3\n\
    \addx 5\n\
    \addx -1\n\
    \addx -8\n\
    \addx 13\n\
    \addx 4\n\
    \noop\n\
    \addx -1\n\
    \addx 5\n\
    \addx -1\n\
    \addx 5\n\
    \addx -1\n\
    \addx 5\n\
    \addx -1\n\
    \addx 5\n\
    \addx -1\n\
    \addx -35\n\
    \addx 1\n\
    \addx 24\n\
    \addx -19\n\
    \addx 1\n\
    \addx 16\n\
    \addx -11\n\
    \noop\n\
    \noop\n\
    \addx 21\n\
    \addx -15\n\
    \noop\n\
    \noop\n\
    \addx -3\n\
    \addx 9\n\
    \addx 1\n\
    \addx -3\n\
    \addx 8\n\
    \addx 1\n\
    \addx 5\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \addx -36\n\
    \noop\n\
    \addx 1\n\
    \addx 7\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \addx 2\n\
    \addx 6\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \addx 1\n\
    \noop\n\
    \noop\n\
    \addx 7\n\
    \addx 1\n\
    \noop\n\
    \addx -13\n\
    \addx 13\n\
    \addx 7\n\
    \noop\n\
    \addx 1\n\
    \addx -33\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \addx 2\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \addx 8\n\
    \noop\n\
    \addx -1\n\
    \addx 2\n\
    \addx 1\n\
    \noop\n\
    \addx 17\n\
    \addx -9\n\
    \addx 1\n\
    \addx 1\n\
    \addx -3\n\
    \addx 11\n\
    \noop\n\
    \noop\n\
    \addx 1\n\
    \noop\n\
    \addx 1\n\
    \noop\n\
    \noop\n\
    \addx -13\n\
    \addx -19\n\
    \addx 1\n\
    \addx 3\n\
    \addx 26\n\
    \addx -30\n\
    \addx 12\n\
    \addx -1\n\
    \addx 3\n\
    \addx 1\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \addx -9\n\
    \addx 18\n\
    \addx 1\n\
    \addx 2\n\
    \noop\n\
    \noop\n\
    \addx 9\n\
    \noop\n\
    \noop\n\
    \noop\n\
    \addx -1\n\
    \addx 2\n\
    \addx -37\n\
    \addx 1\n\
    \addx 3\n\
    \noop\n\
    \addx 15\n\
    \addx -21\n\
    \addx 22\n\
    \addx -6\n\
    \addx 1\n\
    \noop\n\
    \addx 2\n\
    \addx 1\n\
    \noop\n\
    \addx -10\n\
    \noop\n\
    \noop\n\
    \addx 20\n\
    \addx 1\n\
    \addx 2\n\
    \addx 2\n\
    \addx -6\n\
    \addx -11\n\
    \noop\n\
    \noop\n\
    \noop\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 13140)]

-- Part Two

width :: Int
width = 40

pixel :: State -> Bool
pixel (clock, x) = abs ((clock - 1) `mod` width - x) <= 1

drawScreen :: [Bool] -> String
drawScreen = unlines . takes width . map drawPixel

drawPixel :: Bool -> Char
drawPixel True = '#'
drawPixel False = '.'

solve2 :: Input -> String
solve2 = drawScreen . map pixel . values

tests2 :: [(String, String)]
tests2 = [(testInput, "\
    \##..##..##..##..##..##..##..##..##..##..\n\
    \###...###...###...###...###...###...###.\n\
    \####....####....####....####....####....\n\
    \#####.....#####.....#####.....#####.....\n\
    \######......######......######......####\n\
    \#######.......#######.......#######.....\n")]

main :: IO ()
main = do
    s <- readFile "input/10.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    putStr (solve2 input)
