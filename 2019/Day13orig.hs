module Main where

import Utilities
import Geometry
import Intcode
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

-- Input processing

type Input = Memory

parse :: String -> Input
parse = readMemory

-- Part One

data Tile = Empty | Wall | Block | Paddle | Ball
    deriving (Enum, Eq, Ord, Show)

showTile :: Tile -> Char
showTile Empty = '.'
showTile Wall = '|'
showTile Block = '@'
showTile Paddle = '-'
showTile Ball = 'o'

type Screen = Map Point2 Tile

showScreen :: Screen -> String
showScreen = showGrid '.' . fmap showTile

initScreen ::Screen
initScreen = Map.empty

paint :: [Int] -> Screen
paint = foldl paintOne initScreen . takes 3
  where
    paintOne s [x, y, tile] = Map.insert (Point2 x y) (toEnum tile) s
    paintOne _ _ = error "unbalanced instructions"

-- Intcode program as a function on lists of Ints
intFunction :: Memory -> [Int] -> [Int]
intFunction mem = map fromValue . streamFunction mem . map toValue

solve1 :: Input -> Int
solve1 = length . filter (== Block) . Map.elems . paint . flip intFunction []

-- Part Two

data OutputInstruction = Paint Point2 Tile | Score Int
    deriving Show

decode :: [Int] -> [OutputInstruction]
decode = map dec . takes 3
  where
    dec [x, y, v]
      | x == -1 && y == 0 = Score v
      | otherwise = Paint (Point2 x y) (toEnum v)
    dec _ = error "unbalanced instructions"

-- The screen is handy for debugging, but we only need the paddle position
-- (to choose a joystick position when the ball moves) and the score.
data Game = Game {
    screen :: Screen,
    paddlePos :: Maybe Point2,
    score :: Int
    }

showGame :: Game -> String
showGame g = showScreen (screen g) ++ "Score: " ++ show (score g) ++ "\n"

initGame :: Game
initGame = Game {
    screen = initScreen,
    paddlePos = Nothing,
    score = 0
    }

updateGame :: Game -> OutputInstruction -> Game
updateGame g (Paint p Paddle) =
    g { screen = Map.insert p Paddle (screen g), paddlePos = Just p }
updateGame g (Paint p l) = g { screen = Map.insert p l (screen g) }
updateGame g (Score v) = g { score = v }

-- A joystick move whenever the ball moves, making the paddle track the ball.
-- We know that a joystick position is required each time the ball is
-- drawn, but in a stream model we don't know when they will be demanded.
-- In particular, when the ball is first drawn, we don't know where the
-- paddle is yet, and we don't know when the joystick position is required.
-- Fortunately there's enough slack in the setup to not move at first.
joystick :: Game -> OutputInstruction -> Maybe Int
joystick g (Paint (Point2 bx _) Ball) =
    Just $ case paddlePos g of
        Just (Point2 px _) -> signum (bx - px)
        Nothing -> 0
joystick _ _ = Nothing

finalScore :: Memory -> Int
finalScore mem = score last_g
  where
    (last_g, mb_moves) = mapAccumL step initGame outputs
    outputs = runArcade mem (catMaybes mb_moves)
    step g instr = (updateGame g instr, joystick g instr)

-- output of the arcade game, given joystick moves as input
runArcade :: Memory -> [Int] -> [OutputInstruction]
runArcade mem moves = decode (intFunction (deposit mem) moves)

deposit :: Memory -> Memory
deposit = setMemory 0 2

solve2 :: Input -> Int
solve2 = finalScore

-- full history, for tracing
gameHistory :: Memory -> [Game]
gameHistory mem = fmap fst gms
  where
    gms = snd $ mapAccumL step initGame outputs
    outputs = runArcade mem (catMaybes (fmap snd gms))
    step g instr = (g', (g', joystick g instr))
      where
        g' = updateGame g instr

main :: IO ()
main = do
    s <- readFile "input/13.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
