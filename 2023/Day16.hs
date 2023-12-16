module Main where

import Geometry
import Graph
import Utilities
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Input processing

data Element =
    Empty | SW_NE_Mirror | NW_SE_Mirror | EW_Splitter | NS_Splitter
    deriving (Eq, Show)
type Elements = Map Position Element

getElement :: Position -> Elements -> Element
getElement = Map.findWithDefault Empty

type Input = (AABox Position, Elements)

parse :: String -> Input
parse s = (box, elements)
  where
    pcs = readGrid s
    box = boundingBox (map fst pcs)
    elements = Map.fromList
        [(p, e) | (p, c) <- pcs, let e = charToElement c, e /= Empty]

charToElement :: Char -> Element
charToElement '.' = Empty
charToElement '/' = SW_NE_Mirror
charToElement '\\' = NW_SE_Mirror
charToElement '-' = EW_Splitter
charToElement '|' = NS_Splitter
charToElement _ = error "unexpected character"

-- Part One

-- Directions

data Direction = N | E | S | W
    deriving (Bounded, Enum, Eq, Ord, Show)
 
-- one position in the direction
oneStep :: Direction -> Position
oneStep N = Position 0 (-1)
oneStep E = Position 1 0
oneStep S = Position 0 1
oneStep W = Position (-1) 0

type Beam = (Position, Direction)

-- start from the top left corner
startBeam :: Beam
startBeam = (zero, E)

-- one step in moving a beam
move :: AABox Position -> Elements -> Beam -> [Beam]
move box elements (p, d) =
    [(p', d') |
        d' <- deflect d (getElement p elements),
        let p' = p .+. oneStep d',
        inBox p' box]

-- deflect a beam direction off a mirror or splitter
deflect :: Direction -> Element -> [Direction]
deflect N SW_NE_Mirror = [E]
deflect S SW_NE_Mirror = [W]
deflect E SW_NE_Mirror = [N]
deflect W SW_NE_Mirror = [S]
deflect N NW_SE_Mirror = [W]
deflect S NW_SE_Mirror = [E]
deflect E NW_SE_Mirror = [S]
deflect W NW_SE_Mirror = [N]
deflect N EW_Splitter = [E, W]
deflect S EW_Splitter = [E, W]
deflect E NS_Splitter = [N, S]
deflect W NS_Splitter = [N, S]
deflect d _ = [d] -- empty or end-on to a splitter

-- number of energized positions from starting beam
energized :: AABox Position -> Elements -> Beam -> Int
energized box elements beam =
    Set.size $ Set.fromList $ map fst $ concat $ bfs (move box elements) [beam]

solve1 :: Input -> Int
solve1 (box, elements) = energized box elements startBeam

testInput :: String
testInput = "\
    \.|...\\....\n\
    \|.-.\\.....\n\
    \.....|-...\n\
    \........|.\n\
    \..........\n\
    \.........\\\n\
    \..../.\\\\..\n\
    \.-.-/..|..\n\
    \.|....-|.\\\n\
    \..//.|....\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 46)]

-- Part Two

-- beams starting from all the edge positions of the box
startBeams :: AABox Position -> [Beam]
startBeams box =
    [(Position x 0, S) | x <- [0..max_x]] ++
    [(Position x max_y, N) | x <- [0..max_x]] ++
    [(Position 0 y, E) | y <- [0..max_y]] ++
    [(Position max_x y, W) | y <- [0..max_y]]
  where
    Position max_x max_y = maxCorner box

solve2 :: Input -> Int
solve2 (box, elements) =
    maximum $ map (energized box elements) $ startBeams box

tests2 :: [(String, Int)]
tests2 = [(testInput, 51)]

main :: IO ()
main = do
    s <- readFile "input/16.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
