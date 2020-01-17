module Main where

import Cartesian
import Graph
import Utilities
import Data.Bits
import Data.Char
import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = (Maze, Position)

neighbours :: Position -> [Position]
neighbours (Position x y) =
    [Position (x+1) y, Position x (y+1), Position (x-1) y, Position x (y-1)]

type Door = Int

data Maze = Maze {
    passages :: Set Position,
    keys :: Map Position Door,
    doors :: Map Position Door
    }
    deriving Show

parse :: String -> Input
parse s = (maze, pos)
  where
    grid = [(p, c) | (p, c) <- readGrid s, c /= '#']
    features = [(p, c) | (p, c) <- grid, c /= '.']
    maze = Maze {
        passages = Set.fromList (map fst grid),
        keys = Map.fromList [(p, ord c - ord 'a') |
            (p, c) <- features, isLower c],
        doors = Map.fromList [(p, ord c - ord 'A') |
            (p, c) <- features, isUpper c]
        }
    pos = head [p | (p, c) <- features, c == '@']

-- Part One

-- For each startpoint, door or key in the original layout, the number
-- of steps to other doors or keys that are reachable without passing
-- through any door.
bigSteps :: Maze -> [Position] -> Map Position [(Int, Position)]
bigSteps m starts =
    Map.fromList [(p, reachable p) | p <- starts ++ Set.toList waypoints]
  where
    waypoints = Set.union (Map.keysSet (keys m)) (Map.keysSet (doors m))
    reachable p = [(depth, dest) |
        -- first step is special because next won't go past doors
        (depth, dests) <- (1, ns):zip [2..] (tail (bfs next (p:ns))),
        dest <- dests,
        Set.member dest waypoints]
      where
        ns = open p
    open p = [n | n <- neighbours p, Set.member n (passages m)]
    next p -- like open, except won't go anywhere from a door
      | Map.member p (doors m) = []
      | otherwise = open p

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
    collected :: !Keyring,
    position :: !Position
    }
    deriving (Eq, Ord, Show)

initState :: Position -> State
initState p = State noKeys p

showState :: Maze -> State -> String
showState m s = showStateAux m [position s] (collected s)

showStateAux :: Maze -> [Position] -> Keyring -> String
showStateAux m ps coll = showGrid '#' $
    Map.fromList [(p, '@') | p <- ps] `Map.union`
    fmap key (Map.filter live (keys m)) `Map.union`
    fmap door (Map.filter live (doors m)) `Map.union`
    Map.fromSet (const '.') (passages m)
  where
    key d = chr (d + ord 'a')
    door d = chr (d + ord 'A')
    live d = not (hasKey coll d)

solve1 :: Input -> Int
solve1 (m, p) =
    fst $ head $ dropWhile (not . finished . snd) $
        shortestPaths (nextState m graph) $ initState p
  where
    graph = bigSteps m [p]
    ks = allKeys m
    finished s = collected s == ks

-- Move to either an open door or a new key
nextState :: Maze -> Map Position [(Int, Position)] -> State -> [(Int, State)]
nextState m steps (State coll pos) =
    [(dist, State coll' p) |
        (dist, p) <- steps!pos,
        let mb_new_key = Map.lookup p new_keys,
        isJust mb_new_key || Map.member p open_doors,
        let coll' = maybe coll (flip addKey coll) mb_new_key]
  where
    new_keys = Map.filter (not . hasKey coll) (keys m)
    open_doors = Map.filter (hasKey coll) (doors m)

-- all the keys in the maze
allKeys :: Maze -> Keyring
allKeys m = foldr addKey noKeys (Map.elems (keys m))

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
    collected2 :: !Keyring,
    positions :: ![Position]
    }
    deriving (Eq, Ord, Show)

showState2 :: Maze -> State2 -> String
showState2 m s = showStateAux m (positions s) (collected2 s)

-- transformation of the maze into four quadrants for part 2
splitMaze :: (Maze, State) -> (Maze, State2)
splitMaze (m, State coll p) = (m', State2 coll (corners p))
  where
    m' = m { passages = Set.difference (passages m) new_walls }
    new_walls = Set.fromList (p : neighbours p)

corners :: Position -> [Position]
corners (Position x y) =
    [Position (x-1) (y-1), Position (x-1) (y+1),
     Position (x+1) (y-1), Position (x+1) (y+1)]

solve2 :: Input -> Int
solve2 (m0, p) =
    fst $ head $ dropWhile (not . finished . snd) $ shortestPaths (nextState2 m graph) s0
  where
    (m, s0) = splitMaze (m0, initState p)
    graph = bigSteps m (positions s0)
    ks = allKeys m
    finished s = collected2 s == ks

-- Move to either an open door or a new key
nextState2 :: Maze -> Map Position [(Int, Position)] -> State2 -> [(Int, State2)]
nextState2 m steps (State2 coll ps) =
    [(dist, State2 coll' ps') |
        (front, pos:back) <- splits ps,
        (dist, p) <- steps!pos,
        let mb_new_key = Map.lookup p new_keys,
        isJust mb_new_key || Map.member p open_doors,
        let coll' = maybe coll (flip addKey coll) mb_new_key,
        let ps' = front++(p:back)]
  where
    new_keys = Map.filter (not . hasKey coll) (keys m)
    open_doors = Map.filter (hasKey coll) (doors m)

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
