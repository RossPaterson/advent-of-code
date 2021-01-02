module Main where

import Utilities
import Graph
import Geometry
import Data.Char
import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = Maze

data Maze = Maze {
    passages :: Set Position,
    portals :: [Portal],
    start :: Position,
    finish :: Position
    }
    deriving Show

data Portal = Portal { name :: String, inner :: Position, outer :: Position }
    deriving (Eq, Ord, Show)

parse :: String -> Input
parse s = Maze {
    passages = open,
    portals = [portal n p1 p2 | (n, [p1, p2]) <- Map.toList doors],
    start = head (doors ! "AA"),
    finish = head (doors ! "ZZ")
    }
  where
    doors = Map.fromListWith (++) [([c1, c2], [p']) |
        (p1, c1) <- Map.toList letters,
        d <- dirs,
        let p2 = p1 .+. d,
        c2 <- maybeToList (Map.lookup p2 letters),
        p' <- [p1 .-. d, p2 .+. d],
        Set.member p' open]
    letters = Map.fromList [(p, c) | (p, c) <- grid, isUpper c]
    open = Set.fromList [p | (p, c) <- grid, c == '.']
    grid = [(p, c) | (p, c) <- readGrid s, c == '.' || isUpper c]
    dirs = [Position 1 0, Position 0 1]
    portal n p1 p2
      | outside p2 = Portal n p1 p2
      | otherwise = Portal n p2 p1
    outside (Position x y) = x == minX || x == maxX || y == minY || y == maxY
    minX = minimum [x | Position x _ <- Set.toList open]
    maxX = maximum [x | Position x _ <- Set.toList open]
    minY = minimum [y | Position _ y <- Set.toList open]
    maxY = maximum [y | Position _ y <- Set.toList open]

-- Part One

-- adjacent points regardless of walls
neighbours :: Position -> [Position]
neighbours pos = map (pos .+.) unitVectors

-- cached steps from each point
stepMap :: Maze -> Map Position [Position]
stepMap maze = Map.fromSet (stepsFrom maze) (passages maze)

-- steps from a point, either moving in a passage or passing through
-- a portal in either direction
stepsFrom :: Maze -> Position -> [Position]
stepsFrom maze p =
    [p' | p' <- neighbours p, Set.member p' (passages maze)] ++
    [p2 | Portal _ p1 p2 <- portals maze, p1 == p] ++
    [p1 | Portal _ p1 p2 <- portals maze, p2 == p]

solve1 :: Input -> Int
solve1 maze = length $ takeWhile (dest `notElem`) $ bfs steps [begin]
  where
    begin = start maze
    dest = finish maze
    step_map = stepMap maze
    steps p = step_map!p

testInput1 :: String
testInput1 =
    "         A           \n\
    \         A           \n\
    \  #######.#########  \n\
    \  #######.........#  \n\
    \  #######.#######.#  \n\
    \  #######.#######.#  \n\
    \  #######.#######.#  \n\
    \  #####  B    ###.#  \n\
    \BC...##  C    ###.#  \n\
    \  ##.##       ###.#  \n\
    \  ##...DE  F  ###.#  \n\
    \  #####    G  ###.#  \n\
    \  #########.#####.#  \n\
    \DE..#######...###.#  \n\
    \  #.#########.###.#  \n\
    \FG..#########.....#  \n\
    \  ###########.#####  \n\
    \             Z       \n\
    \             Z       \n"

testInput2 :: String
testInput2 =
    "                   A               \n\
    \                   A               \n\
    \  #################.#############  \n\
    \  #.#...#...................#.#.#  \n\
    \  #.#.#.###.###.###.#########.#.#  \n\
    \  #.#.#.......#...#.....#.#.#...#  \n\
    \  #.#########.###.#####.#.#.###.#  \n\
    \  #.............#.#.....#.......#  \n\
    \  ###.###########.###.#####.#.#.#  \n\
    \  #.....#        A   C    #.#.#.#  \n\
    \  #######        S   P    #####.#  \n\
    \  #.#...#                 #......VT\n\
    \  #.#.#.#                 #.#####  \n\
    \  #...#.#               YN....#.#  \n\
    \  #.###.#                 #####.#  \n\
    \DI....#.#                 #.....#  \n\
    \  #####.#                 #.###.#  \n\
    \ZZ......#               QG....#..AS\n\
    \  ###.###                 #######  \n\
    \JO..#.#.#                 #.....#  \n\
    \  #.#.#.#                 ###.#.#  \n\
    \  #...#..DI             BU....#..LF\n\
    \  #####.#                 #.#####  \n\
    \YN......#               VT..#....QG\n\
    \  #.###.#                 #.###.#  \n\
    \  #.#...#                 #.....#  \n\
    \  ###.###    J L     J    #.#.###  \n\
    \  #.....#    O F     P    #.#...#  \n\
    \  #.###.#####.#.#####.#####.###.#  \n\
    \  #...#.#.#...#.....#.....#.#...#  \n\
    \  #.#####.###.###.#.#.#########.#  \n\
    \  #...#.#.....#...#.#.#.#.....#.#  \n\
    \  #.###.#####.###.###.#.#.#######  \n\
    \  #.#.........#...#.............#  \n\
    \  #########.###.###.#############  \n\
    \           B   J   C               \n\
    \           U   P   P               \n"

tests1 :: [(String, Int)]
tests1 = [(testInput1, 23), (testInput2, 58)]

-- Part Two

-- cached steps from each point
stepMap2 :: Maze -> Map Position [(Position, Int)]
stepMap2 maze = Map.fromSet (stepsFrom2 maze) (passages maze)

-- all points and level changes immediately reachable from a point,
-- either moving in a passage on the same level (0) or going up (-1)
-- or down (1) through a portal
stepsFrom2 :: Maze -> Position -> [(Position, Int)]
stepsFrom2 maze p =
    [(p', 0) | p' <- neighbours p, Set.member p' (passages maze)] ++
    [(p2, 1) | Portal _ p1 p2 <- portals maze, p1 == p] ++
    [(p1, -1) | Portal _ p1 p2 <- portals maze, p2 == p]

solve2 :: Input -> Int
solve2 maze = length $ takeWhile (dest `notElem`) $ bfs steps [begin]
  where
    begin = (start maze, 0)
    dest = (finish maze, 0)
    step_map = stepMap2 maze
    steps (p, level) = [(p', level') |
        (p', n) <- step_map!p, let level' = level+n, level' >= 0]

testInput3 :: String
testInput3 =
    "             Z L X W       C                 \n\
    \             Z P Q B       K                 \n\
    \  ###########.#.#.#.#######.###############  \n\
    \  #...#.......#.#.......#.#.......#.#.#...#  \n\
    \  ###.#.#.#.#.#.#.#.###.#.#.#######.#.#.###  \n\
    \  #.#...#.#.#...#.#.#...#...#...#.#.......#  \n\
    \  #.###.#######.###.###.#.###.###.#.#######  \n\
    \  #...#.......#.#...#...#.............#...#  \n\
    \  #.#########.#######.#.#######.#######.###  \n\
    \  #...#.#    F       R I       Z    #.#.#.#  \n\
    \  #.###.#    D       E C       H    #.#.#.#  \n\
    \  #.#...#                           #...#.#  \n\
    \  #.###.#                           #.###.#  \n\
    \  #.#....OA                       WB..#.#..ZH\n\
    \  #.###.#                           #.#.#.#  \n\
    \CJ......#                           #.....#  \n\
    \  #######                           #######  \n\
    \  #.#....CK                         #......IC\n\
    \  #.###.#                           #.###.#  \n\
    \  #.....#                           #...#.#  \n\
    \  ###.###                           #.#.#.#  \n\
    \XF....#.#                         RF..#.#.#  \n\
    \  #####.#                           #######  \n\
    \  #......CJ                       NM..#...#  \n\
    \  ###.#.#                           #.###.#  \n\
    \RE....#.#                           #......RF\n\
    \  ###.###        X   X       L      #.#.#.#  \n\
    \  #.....#        F   Q       P      #.#.#.#  \n\
    \  ###.###########.###.#######.#########.###  \n\
    \  #.....#...#.....#.......#...#.....#.#...#  \n\
    \  #####.#.###.#######.#######.###.###.#.#.#  \n\
    \  #.......#.......#.#.#.#.#...#...#...#.#.#  \n\
    \  #####.###.#####.#.#.#.#.###.###.#.###.###  \n\
    \  #.......#.....#.#...#...............#...#  \n\
    \  #############.#.#.###.###################  \n\
    \               A O F   N                     \n\
    \               A A D   M                     \n"

tests2 :: [(String, Int)]
tests2 = [(testInput1, 26), (testInput3, 396)]

main :: IO ()
main = do
    s <- readFile "input/20.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
