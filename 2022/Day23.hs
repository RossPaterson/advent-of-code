module Main where

import Utilities
import Geometry
import Control.Monad
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = Set Position

parse :: String -> Input
parse s = Set.fromList [p | (p, c) <- readGrid s, c == '#']

-- Part One

data Direction = N | NE | E | SE | S | SW | W | NW
    deriving (Bounded, Enum, Eq, Ord, Show)

oneStep :: Direction -> Position
oneStep N = Position 0 (-1)
oneStep NE = Position 1 (-1)
oneStep E = Position 1 0
oneStep SE = Position 1 1
oneStep S = Position 0 1
oneStep SW = Position (-1) 1
oneStep W = Position (-1) 0
oneStep NW = Position (-1) (-1)

type Directions = [Direction]

initDirss :: [Directions]
initDirss = [[N, NE, NW], [S, SE, SW], [W, NW, SW], [E, NE, SE]]

-- Phase one: propose moves
proposals :: Set Position -> [Directions] -> Map Position (Set Position)
proposals ps dirss =
    Map.fromListWith Set.union
        [(target, Set.singleton source) |
            source <- Set.elems ps,
            target <- maybeToList (propose ps dirss source)]

propose :: Set Position -> [Directions] -> Position -> Maybe Position
propose ps dirss p
  | and [not (Set.member (p .+. oneStep d) ps) | d <- allValues] = Nothing
  | otherwise = msum [consider ps p dirs | dirs <- dirss]

consider :: Set Position -> Position -> Directions -> Maybe Position
consider ps p dirs
  | and [not (Set.member dest ps) | dest <- dests] = listToMaybe dests
  | otherwise = Nothing
  where
    dests = [p .+. oneStep dir | dir <- dirs]

-- Phase two: execute proposed moves that do not collide
move :: Set Position -> [Directions] -> Maybe (Set Position)
move ps dirss
  | Map.null can_move = Nothing
  | otherwise = Just (Set.union (Set.difference ps sources) targets)
  where
    can_move = Map.filter isSingleton (proposals ps dirss)
    targets = Map.keysSet can_move
    sources = Set.unions (Map.elems can_move)

isSingleton :: Set a -> Bool
isSingleton s = Set.size s == 1

-- current positions and order in which to try directions
type State = (Set Position, [Directions])

initState :: Set Position -> State
initState ps = (ps, initDirss)

oneRound :: State -> Maybe State
oneRound (ps, dirss) = do
    ps' <- move ps dirss
    return (ps', tail dirss ++ [head dirss])

-- list of states from start until no change
states :: Set Position -> [State]
states ps = iterateWhileJust oneRound (initState ps)

countEmpty :: Set Position -> Int
countEmpty ps = boxSize (boundingBox ps) - Set.size ps

solve1 :: Input -> Int
solve1 ps = countEmpty (fst (states ps!!10))

smallInput :: String
smallInput = "\
    \.....\n\
    \..##.\n\
    \..#..\n\
    \.....\n\
    \..##.\n\
    \.....\n"

testInput :: String
testInput = "\
    \....#..\n\
    \..###.#\n\
    \#...#.#\n\
    \.#...##\n\
    \#.###..\n\
    \##.#.##\n\
    \.#..#..\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 110)]

-- Part Two

solve2 :: Input -> Int
solve2 = length . states

tests2 :: [(String, Int)]
tests2 = [(testInput, 20)]

main :: IO ()
main = do
    s <- readFile "input/23.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
