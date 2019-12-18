module Main where

import Utilities
import Data.Bits
import Data.Char
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = (Maze, Point)

type Point = (Int, Int)

neighbours :: Point -> [Point]
neighbours (x, y) = [(x+1, y), (x, y+1), (x-1, y), (x, y-1)]

type Door = Int

data Maze = Maze {
    passages :: Set Point,
    keys :: Map Point Door,
    doors :: Map Point Door
    }
    deriving Show

parse :: String -> Input
parse s = (maze, pos)
  where
    grid = [((x, y), c) | (y, l) <- zip [0..] (lines s), (x, c) <- zip [0..] l, c /= '#']
    open = [(p, c) | (p, c) <- grid, c /= '.']
    maze = Maze {
        passages = Set.fromList (map fst grid),
        keys = Map.fromList [(p, ord c - ord 'a') | (p, c) <- open, isLower c],
        doors = Map.fromList [(p, ord c - ord 'A') | (p, c) <- open, isUpper c]
        }
    pos = head [p | (p, c) <- open, c == '@']

-- Part One

-- a bit set of keys
newtype Keyring = Keyring Int
    deriving (Eq, Ord, Show)

noKeys :: Keyring
noKeys = Keyring 0

hasKey :: Keyring -> Door -> Bool
hasKey (Keyring ds) d = testBit ds d

addKey :: Door -> Keyring -> Keyring
addKey d (Keyring ds) = Keyring (setBit ds d)

data State = State {
    position :: !Point,
    collected :: !Keyring
    }
    deriving (Eq, Ord, Show)

initState :: Point -> State
initState p = State p noKeys

showState :: Maze -> State -> String
showState m (State p coll) = showStateAux m [p] coll

showStateAux :: Maze -> [Point] -> Keyring -> String
showStateAux m ps coll =
    unlines [[showPosition (x, y) | x <- [minX..maxX]] | y <- [minY..maxY]]
  where
    open = passages m
    showPosition p
      | not (Set.member p open) = '#'
      | elem p ps = '@'
      | otherwise =
        case Map.lookup p (keys m) of
        Just d | not (hasKey coll d) -> chr (d + ord 'a')
        _ -> case Map.lookup p (doors m) of
            Just d | not (hasKey coll d) -> chr (d + ord 'A')
            _ -> '.'
    minX = minimum (map fst (Set.toList open)) - 1
    maxX = maximum (map fst (Set.toList open)) + 1
    minY = minimum (map snd (Set.toList open)) - 1
    maxY = maximum (map snd (Set.toList open)) + 1

solve1 :: Input -> Int
solve1 (m, p) =
    fst $ head $ dropWhile (not . finished . snd) $
        shortestPaths (getKey m) $ initState p
  where
    ks = allKeys m
    finished (State _ coll) = coll == ks

-- all the keys in the maze
allKeys :: Maze -> Keyring
allKeys m = foldr addKey noKeys (Map.elems (keys m))

-- move from the current state to a new key
getKey :: Maze -> State -> [(Int, State)]
getKey m (State pos coll) =
    [(n, State p (addKey d coll)) | (n, p, d) <- keyDistances m coll pos]

-- shortest distance and position of each key reachable from p
keyDistances :: Maze -> Keyring -> Point -> [(Int, Point, Door)]
keyDistances m ds pos =
    [(n, p, d) |
        (n, ps) <- zip [0..] (bfs (steps m ds) [pos]),
        p <- ps,
        d <- maybeToList (Map.lookup p (keys m)),
        not (hasKey ds d)]

-- neighbouring points that are not walls or locked doors
steps :: Maze -> Keyring -> Point -> [Point]
steps m coll pos = [p | p <- neighbours pos, isOpen m coll p]

-- a cell is open if it is not a wall or a locked door
isOpen :: Maze -> Keyring -> Point -> Bool
isOpen m ds p =
    Set.member p (passages m) && maybe True (hasKey ds) (Map.lookup p (doors m))

testInput1 :: String
testInput1 =
    "#########\n\
    \#b.A.@.a#\n\
    \#########\n"

testInput2 :: String
testInput2 =
    "########################\n\
    \#f.D.E.e.C.b.A.@.a.B.c.#\n\
    \######################.#\n\
    \#d.....................#\n\
    \########################\n"

testInput3 :: String
testInput3 =
    "########################\n\
    \#...............b.C.D.f#\n\
    \#.######################\n\
    \#.....@.a.B.c.d.A.e.F.g#\n\
    \########################\n"

testInput4 :: String
testInput4 =
    "#################\n\
    \#i.G..c...e..H.p#\n\
    \########.########\n\
    \#j.A..b...f..D.o#\n\
    \########@########\n\
    \#k.E..a...g..B.n#\n\
    \########.########\n\
    \#l.F..d...h..C.m#\n\
    \#################\n"

testInput5 :: String
testInput5 =
    "########################\n\
    \#@..............ac.GI.b#\n\
    \###d#e#f################\n\
    \###A#B#C################\n\
    \###g#h#i################\n\
    \########################\n"

tests1 :: [(String, Int)]
tests1 = [
    (testInput1, 8), (testInput2, 86), (testInput3, 132),
    (testInput4, 136), (testInput5, 81)]

-- Part Two

-- now there are multiple searchers
data State2 = State2 {
    positions :: ![Point],
    collected2 :: !Keyring
    }
    deriving (Eq, Ord, Show)

showState2 :: Maze -> State2 -> String
showState2 m (State2 ps coll) = showStateAux m ps coll

-- transformation of the maze into four quadrants for part 2
splitMaze :: (Maze, State) -> (Maze, State2)
splitMaze (m, State p coll) = (m', State2 (corners p) coll)
  where
    m' = m { passages = Set.difference (passages m) new_walls }
    new_walls = Set.fromList (p : neighbours p)

corners :: Point -> [Point]
corners (x, y) = [(x-1, y-1), (x-1, y+1), (x+1, y-1), (x+1, y+1)]

solve2 :: Input -> Int
solve2 (m0, p) =
    fst $ head $ dropWhile (not . finished . snd) $ shortestPaths (getKey2 m) s
  where
    (m, s) = splitMaze (m0, initState p)
    ks = allKeys m
    finished (State2 _ coll) = coll == ks

-- move one of the bots to a new key
getKey2 :: Maze -> State2 -> [(Int, State2)]
getKey2 m (State2 ps coll) =
    [(n, State2 (front++(p':back)) (addKey d coll)) |
        (front, p:back) <- zip (inits ps) (tails ps),
        (n, p', d) <- keyDistances m coll p]

testInput6 :: String
testInput6 =
    "#######\n\
    \#a.#Cd#\n\
    \##...##\n\
    \##.@.##\n\
    \##...##\n\
    \#cB#Ab#\n\
    \#######\n"

testInput7 :: String
testInput7 =
    "###############\n\
    \#d.ABC.#.....a#\n\
    \######...######\n\
    \######.@.######\n\
    \######...######\n\
    \#b.....#.....c#\n\
    \###############\n"

testInput8 :: String
testInput8 =
    "#############\n\
    \#DcBa.#.GhKl#\n\
    \#.###...#I###\n\
    \#e#d#.@.#j#k#\n\
    \###C#...###J#\n\
    \#fEbA.#.FgHi#\n\
    \#############\n"

testInput9 :: String
testInput9 =
    "#############\n\
    \#g#f.D#..h#l#\n\
    \#F###e#E###.#\n\
    \#dCba...BcIJ#\n\
    \#####.@.#####\n\
    \#nK.L...G...#\n\
    \#M###N#H###.#\n\
    \#o#m..#i#jk.#\n\
    \#############\n"

tests2 :: [(String, Int)]
tests2 = [
    (testInput6, 8),
    (testInput7, 24),
    (testInput8, 32),
    (testInput9, 72)]

main :: IO ()
main = do
    s <- readFile "input/18.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)