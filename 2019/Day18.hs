module Main where

import Geometry
import qualified CompactSet as CompactSet
import Graph
import Utilities
import Control.Monad
import Data.Char
import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = (Maze, Position)

neighbours :: Position -> [Position]
neighbours pos = map (pos .+.) cardinalDirections

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
        keys = Map.fromList [(p, ord c - ord 'a') |
            (p, c) <- features, isLower c],
        doors = Map.fromList [(p, ord c - ord 'A') |
            (p, c) <- features, isUpper c]
        }
    pos = head [p | (p, c) <- features, c == '@']

-- Part One

-- a waypoint is a startpoint, door or key in the original layout
data Waypoint = Start !Position | Door !DoorId | Key !DoorId
    deriving (Eq, Ord, Show)

-- weighted graph
type Graph a = Map a [(Int, a)]

-- graph of distances between waypoints without passing through any door.
waypointGraph :: Maze -> [Position] -> Graph Waypoint
waypointGraph m starts = fmap reachable waypointLocs
  where
    waypointLocs = Map.fromList $
        [(Start p, p) | p <- starts] ++
        [(Door k, p) | (p, k) <- Map.assocs (doors m)] ++
        [(Key k, p) | (p, k) <- Map.assocs (keys m)]
    reachable p = [(depth, wp) |
        -- first step is special because next won't go past doors
        (depth, dests) <- (1, ns):zip [2..] (tail (bfs next (p:ns))),
        dest <- dests,
        wp <- maybeToList (waypoint dest)]
      where
        ns = open p
    waypoint p =
        fmap Door (Map.lookup p (doors m)) `mplus`
        fmap Key (Map.lookup p (keys m))
    open p = [n | n <- neighbours p, Set.member n (passages m)]
    next p -- like open, except won't go anywhere from a door
      | Map.member p (doors m) = []
      | otherwise = open p

-- a set of keys
type Keyring = CompactSet.Set DoorId

data State = State {
    collected :: !Keyring,
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
wpPosition m (Door d) = head [p | (p, k) <- Map.assocs (doors m), k == d]
wpPosition m (Key d) = head [p | (p, k) <- Map.assocs (keys m), k == d]

showStateAux :: Maze -> [Position] -> Keyring -> String
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
        shortestPaths (nextState graph) $ initState p
  where
    graph = waypointGraph m [p]
    ks = allKeys m
    finished s = collected s == ks

-- Move to either an open door or an uncollected key
nextState :: Graph Waypoint -> State -> [(Int, State)]
nextState graph (State coll p) =
    [(dist, State (updateKeyring p' coll) p') |
        (dist, p') <- graph!p, isTarget coll p']

-- update keyring on arriving at the waypoint
updateKeyring :: Waypoint -> Keyring -> Keyring
updateKeyring (Key k) ks = CompactSet.insert k ks
updateKeyring _ ks = ks

-- waypoint is an open door or uncollected key
isTarget :: Keyring -> Waypoint -> Bool
isTarget ks (Key k) = not (CompactSet.member k ks) -- uncollected key
isTarget ks (Door k) = CompactSet.member k ks -- open door
isTarget _ _ = False

-- all the keys in the maze
allKeys :: Maze -> Keyring
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
    collected2 :: !Keyring,
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
        shortestPaths (nextState2 graph) s0
  where
    (m, ps) = splitMaze (m0, p)
    s0 = initState2 ps
    graph = waypointGraph m ps
    ks = allKeys m
    finished s = collected2 s == ks

-- Move one of the robots to either an open door or an uncollected key
nextState2 :: Graph Waypoint -> State2 -> [(Int, State2)]
nextState2 graph (State2 coll ps) =
    [(dist, State2 coll' ps') |
        (front, p:back) <- splits ps,
        (dist, p') <- graph!p,
        isTarget coll p',
        let coll' = updateKeyring p' coll,
        let ps' = front++(p':back)]

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
