module Main where

import Utilities
import Cartesian
import Data.Foldable
import Data.List
import Data.Maybe
import Data.Ord
import Data.Map (Map, (!))
import qualified Data.Map as Map

-- Input processing

type Input = (Track, Carts)

type Track = Map Position Piece
data Piece = Horiz | Vert | Intersection | CurveDown | CurveUp
  deriving Show

type Carts = Map Position CartState
type Cart = (Position, CartState)
data CartState = CartState Direction TurnDir
  deriving Show

-- directions of travel, in clockwise order
data Direction = U | R | D | L
  deriving (Show, Eq, Bounded, Enum)

-- turning directions, in the order that carts cycle through them
data TurnDir = TurnLeft | Straight | TurnRight
  deriving (Show, Eq, Bounded, Enum)

parse :: String -> Input
parse s =
    (Map.fromList [(p, piece c) | (p, c) <- pcs, c /= ' '],
     Map.fromList [(p, CartState dir TurnLeft) |
        (p, c) <- pcs, dir <- maybeToList (direction c)])
  where
    pcs = readGrid s

piece :: Char -> Piece
piece '-' = Horiz
piece '<' = Horiz
piece '>' = Horiz
piece '|' = Vert
piece 'v' = Vert
piece '^' = Vert
piece '+' = Intersection
piece '/' = CurveUp
piece '\\' = CurveDown
piece _ = error "bad piece"

direction :: Char -> Maybe Direction
direction '<' = Just L
direction '>' = Just R
direction '^' = Just U
direction 'v' = Just D
direction _ = Nothing

-- Part One

-- position of the first crash
solve1 :: Input -> Position
solve1 (t, cs) = whileRight (moveAll t) cs

-- try to move all the carts by one step, returning either the position
-- of a crash or an updated cart collection
moveAll :: Track -> Carts -> Either Position Carts
moveAll t cs = foldlM (moveOne t) cs (moveOrder cs)

-- try to move a cart by one step, returning either the position of a
-- crash or an updated cart collection
moveOne :: Track -> Carts -> Cart -> Either Position Carts
moveOne t cs (p, dir)
  | Map.member p' cs = Left p' -- something at target position, i.e. crash
  | otherwise = Right (Map.insert p' dirs' (Map.delete p cs))
  where
    (p', dirs') = moveCart t (p, dir)

-- order in which to move the carts
moveOrder :: Carts -> [Cart]
moveOrder = sortBy (comparing key) . Map.toList
  where
    key (Position x y, _) = (y, x)

moveCart :: Track -> Cart -> Cart
moveCart t (p, o) = (p .+. move dir, o')
  where
    o'@(CartState dir _) = turns (t!p) o

move :: Direction -> Position
move U = Position 0 (-1)
move D = Position 0 1
move L = Position (-1) 0
move R = Position 1 0

turns :: Piece -> CartState -> CartState
turns Intersection (CartState dir td) = CartState (turn td dir) (succWrap td)
turns CurveDown (CartState dir td) = CartState (curveDown dir) td
turns CurveUp (CartState dir td) = CartState (curveUp dir) td
turns _ o = o

turn :: TurnDir -> Direction -> Direction
turn TurnLeft = predWrap
turn Straight = id
turn TurnRight = succWrap

curveDown :: Direction -> Direction
curveDown R = D
curveDown D = R
curveDown L = U
curveDown U = L

curveUp :: Direction -> Direction
curveUp R = U
curveUp D = L
curveUp L = D
curveUp U = R

testStraight :: String
testStraight =
    "|\n\
    \v\n\
    \|\n\
    \|\n\
    \|\n\
    \^\n\
    \|\n"

testInput :: String
testInput =
    "/->-\\        \n\
    \|   |  /----\\\n\
    \| /-+--+-\\  |\n\
    \| | |  | v  |\n\
    \\\-+-/  \\-+--/\n\
    \  \\------/   \n"

tests1 :: [(String, Position)]
tests1 = [(testStraight, Position 0 3), (testInput, Position 7 3)]

-- Part Two

solve2 :: Input -> Position
solve2 (t, cs) =
    head $ Map.keys $ until finished (moveAllRemovingCrashes t) cs

finished :: Carts -> Bool
finished cs = Map.size cs <= 1

-- like moveAll, but remove both carts involved in any crash
moveAllRemovingCrashes :: Track -> Carts -> Carts
moveAllRemovingCrashes t cs =
    foldl (moveOneRemovingCrashes t) cs (moveOrder cs)

-- like moveOne, but remove both carts involved in a crash
moveOneRemovingCrashes :: Track -> Carts -> Cart -> Carts
moveOneRemovingCrashes t cs (p, dir)
  | not (Map.member p cs) = cs -- already removed in a crash
  | Map.member p' cs = Map.delete p' (Map.delete p cs) -- crash
  | otherwise = Map.insert p' dirs' (Map.delete p cs)
  where
    (p', dirs') = moveCart t (p, dir)

testTwoLoops :: String
testTwoLoops = "\
\/>-<\\  \n\
\|   |  \n\
\| /<+-\\\n\
\| | | v\n\
\\\>+</ |\n\
\  |   ^\n\
\  \\<->/\n"

tests2 :: [(String, Position)]
tests2 = [(testTwoLoops, Position 6 4)]

main :: IO ()
main = do
    s <- readFile "input/13.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
