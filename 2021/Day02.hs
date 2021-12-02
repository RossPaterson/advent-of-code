module Main where

import Utilities
import Parser
import Geometry
import Control.Applicative

-- Input processing

type Input = [Command]

data Command = Forward Int | Down Int | Up Int
    deriving Show

parse :: String -> Input
parse = map (runParser command) . lines
  where
    command =
        Forward <$ string "forward " <*> int <|>
        Down <$ string "down " <*> int <|>
        Up <$ string "up " <*> int

-- Part One

move :: Command -> Position
move (Forward n) = Position n 0
move (Down n) = Position 0 n
move (Up n) = Position 0 (-n)

solve1 :: Input -> Int
solve1 = multiply . foldl (.+.) zero . map move

multiply :: Position -> Int
multiply (Position h d) = h*d

testInput :: String
testInput = "\
    \forward 5\n\
    \down 5\n\
    \forward 8\n\
    \up 3\n\
    \down 8\n\
    \forward 2\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 150)]

-- Part Two

data State2 = State2 { position :: Position, aim :: Int }
    deriving Show

start :: State2
start = State2 zero 0

action :: State2 -> Command -> State2
action s (Forward n) = s { position = position s .+. Position n (aim s*n) }
action s (Down n) = s { aim = aim s + n }
action s (Up n) = s { aim = aim s - n }

solve2 :: Input -> Int
solve2 = multiply . position . foldl action start

tests2 :: [(String, Int)]
tests2 = [(testInput, 900)]

main :: IO ()
main = do
    s <- readFile "input/02.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
