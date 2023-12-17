module Main where

import Geometry
import qualified Data.CompactSet as CompactSet
import Graph
import Utilities
import Data.Char
import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = (Maze, Position)

-- identifier of a door and corresponding key
type DoorId = Int

data Maze = Maze {
    passages :: Set Position,
    keys :: Map Position DoorId,
    doors :: Map Position DoorId
    }
    deriving Show

parse :: String -> Input
parse s = (maze, pos)
  where
    grid = [(p, c) | (p, c) <- readGrid s, c /= '#']
    features = [(p, c) | (p, c) <- grid, c /= '.']
    maze = Maze {
        passages = Set.fromList (map fst grid),
        keys = Map.fromList
            [(p, ord c - ord 'a') | (p, c) <- features, isLower c],
        doors = Map.fromList
            [(p, ord c - ord 'A') | (p, c) <- features, isUpper c]
        }
    pos = head [p | (p, c) <- features, c == '@']

-- Part One

neighbours :: Position -> [Position]
neighbours pos = map (pos .+.) unitVectors

-- graph of key locations weighted with distance and keys required to
-- move from the source to the key location
-- (A single set is sufficient because there are no islands in the maze,
-- and thus no alternative routes though other doors.)
type KeyGraph = Map Waypoint [(Int, KeySet, DoorId)]

-- The source of an edge is either a startpoint or a key in the original
-- layout.  (Only keys can be targets: we don't care where we started
-- once we start moving.)
data Waypoint = Start !Position | Key !DoorId
    deriving (Eq, Ord, Show)

-- a set of keys
type KeySet = CompactSet.Set DoorId

-- graph of distances from waypoints to keys without passing over any key.
keyGraph :: Maze -> [Position] -> KeyGraph
keyGraph m starts = fmap adjacent_keys waypoint_locs
  where
    -- map from all waypoints to their locations
    waypoint_locs = Map.fromList $
        [(Start p, p) | p <- starts] ++
        [(Key k, p) | (p, k) <- Map.assocs (keys m)]
    -- keys we can get to from p without going over any other key
    adjacent_keys p = [(depth, door_keys between, k) |
        -- For the first step, we move to any neighbouring passage,
        -- but for subsequent steps we don't move past a key, and we
        -- don't return to p.
        (depth, paths) <-
            zip [1..] ([[n] | n <- ns]:tail (bfsPaths next (p:ns))),
        -- each path is in reverse order, so the end point is the head
        dest:between <- paths,
        -- Is there a key at the end of the path?
        k <- maybeToList (Map.lookup dest (keys m))]
      where
        ns = open p
    open p = [n | n <- neighbours p, Set.member n (passages m)]
    next p -- like open, except won't go anywhere from a key
      | Map.member p (keys m) = []
      | otherwise = open p
    door_keys ps = CompactSet.fromList $ Map.elems $
        Map.restrictKeys (doors m) (Set.fromList ps)

data State = State {
    collected :: !KeySet,
    position :: !Waypoint
    }
    deriving (Eq, Ord, Show)

initState :: Position -> State
initState p = State mempty (Start p)

showState :: Maze -> State -> String
showState m s = showStateAux m [wpPosition m (position s)] (collected s)

-- position of the waypoint in a maze
wpPosition :: Maze -> Waypoint -> Position
wpPosition _ (Start p) = p
wpPosition m (Key d) = head [p | (p, k) <- Map.assocs (keys m), k == d]

showStateAux :: Maze -> [Position] -> KeySet -> String
showStateAux m ps coll = showGrid '#' $
    Map.fromList [(p, '@') | p <- ps] `Map.union`
    fmap key (Map.filter live (keys m)) `Map.union`
    fmap door (Map.filter live (doors m)) `Map.union`
    Map.fromSet (const '.') (passages m)
  where
    key d = chr (d + ord 'a')
    door d = chr (d + ord 'A')
    live d = not (CompactSet.member d coll)

solve1 :: Input -> Int
solve1 (m, p) =
    fst $ head $ dropWhile (not . finished . snd) $
        shortestPaths (nextState graph) [initState p]
  where
    graph = keyGraph m [p]
    ks = allKeys m
    finished s = collected s == ks

-- Move to the location of a key
nextState :: KeyGraph -> State -> [(Int, State)]
nextState graph (State coll p) =
    [(dist, State (CompactSet.insert k coll) (Key k)) |
        (dist, keys_reqd, k) <- graph!p,
        CompactSet.isSubsetOf keys_reqd coll]

-- all the keys in the maze
allKeys :: Maze -> KeySet
allKeys m = CompactSet.fromList (Map.elems (keys m))

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
    collected2 :: !KeySet,
    positions :: ![Waypoint]
    }
    deriving (Eq, Ord, Show)

initState2 :: [Position] -> State2
initState2 ps = State2 mempty (map Start ps)

showState2 :: Maze -> State2 -> String
showState2 m s =
    showStateAux m (map (wpPosition m) (positions s)) (collected2 s)

-- transformation of the maze into four quadrants for part 2
splitMaze :: (Maze, Position) -> (Maze, [Position])
splitMaze (m, p) = (m', new_ps)
  where
    m' = m { passages = Set.difference (passages m) new_walls }
    new_walls = Set.fromList (p : neighbours p)
    new_ps = map (p .+.) corners

solve2 :: Input -> Int
solve2 (m0, p) =
    fst $ head $ dropWhile (not . finished . snd) $
        shortestPaths (nextState2 graph) [s0]
  where
    (m, ps) = splitMaze (m0, p)
    s0 = initState2 ps
    graph = keyGraph m ps
    ks = allKeys m
    finished s = collected2 s == ks

-- Move one of the robots to the location of a key
nextState2 :: KeyGraph -> State2 -> [(Int, State2)]
nextState2 graph (State2 coll ps) =
    [(dist, State2 coll' ps') |
        (front, p:back) <- splits ps,
        (dist, keys_reqd, k) <- graph!p,
        CompactSet.isSubsetOf keys_reqd coll,
        let coll' = CompactSet.insert k coll,
        let ps' = front++(Key k:back)]

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
