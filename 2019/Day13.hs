module Main where

import Utilities
import Cartesian
import Intcode
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

type Screen = Map Position Tile

showScreen :: Screen -> String
showScreen = showGrid '.' . fmap showTile

initScreen ::Screen
initScreen = Map.empty

paint :: [Int] -> Screen
paint = foldl paintOne initScreen . takes 3
  where
    paintOne s [x, y, tile] = Map.insert (Position x y) (toEnum tile) s
    paintOne _ _ = error "unbalanced instructions"

-- Intcode program as a function on lists of Ints
intFunction :: Memory -> [Int] -> [Int]
intFunction mem = map fromValue . streamFunction mem . map toValue

solve1 :: Input -> Int
solve1 = length . filter (== Block) . Map.elems . paint . flip intFunction []

-- Part Two

-- The screen is handy for debugging, but we only need the paddle and ball
-- positions (to choose a joystick position) and the score.
data Game = Game {
    machine :: Automaton,
    screen :: Screen,
    paddlePos :: Maybe Position,
    ballPos :: Maybe Position,
    score :: Int
    }

showGame :: Game -> String
showGame g = showScreen (screen g) ++ "Score: " ++ show (score g) ++ "\n"

initGame :: Memory -> Game
initGame mem = Game {
    machine = automaton (deposit mem),
    screen = initScreen,
    paddlePos = Nothing,
    ballPos = Nothing,
    score = 0
    }

deposit :: Memory -> Memory
deposit = setMemory 0 2

updateGame :: Position -> Tile -> Game -> Game
updateGame p Paddle g =
    g { screen = Map.insert p Paddle (screen g), paddlePos = Just p }
updateGame p Ball g =
    g { screen = Map.insert p Ball (screen g), ballPos = Just p }
updateGame p l g = g { screen = Map.insert p l (screen g) }

-- joystick values: -1 tilt left, 0 neutral, 1 tilt right
-- chosen to move the paddle toward the ball
joystick :: Game -> Int
joystick g = fromMaybe 0 $ do
    Position px _ <- paddlePos g
    Position bx _ <- ballPos g
    return (signum (bx - px))

step :: Game -> Maybe Game
step g = case machine g of
    ReadValue k -> Just (g { machine = k (toValue (joystick g)) })
    WriteValue x (WriteValue y (WriteValue v a))
      | x == -1 -> Just (g' { score = fromValue v })
      | otherwise -> Just (updateGame p (fromValue v) g')
      where
        g' = g { machine = a }
        p = Position (fromValue x) (fromValue y)
    Finish _ -> Nothing
    _ -> error "unbalanced instructions"

solve2 :: Input -> Int
solve2 mem = score $ whileJust step $ initGame mem

main :: IO ()
main = do
    s <- readFile "input/13.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
