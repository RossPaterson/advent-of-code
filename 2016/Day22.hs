module Main where

import Utilities
import Geometry
import Graph
import Parser
import Control.Applicative
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Ord

data Usage = Usage { size :: Int, used :: Int }
  deriving (Show, Eq, Ord)

available :: Usage -> Int
available u = size u - used u

type Input = Map Position Usage

parse :: String -> Input
parse = Map.fromList . map (runParser df_line) . drop 2 . lines
  where
    df_line = (,) <$> node <*> usage
    node = Position <$ string "/dev/grid/node-x" <*> nat <* string "-y" <*> nat
    usage = Usage <$> num 'T' <*> num 'T' <* num 'T' <* num '%'
    num :: Char -> Parser Int
    num c = some space *> nat <* char c

-- Part One --

viable_pairs :: Map Position Usage -> [(Position, Position)]
viable_pairs df =
    [(p1, p2) |
        (p1, u1) <- usage, let needed = used u1, needed /= 0,
        (p2, _) <- takeWhile ((>= needed) . snd) free,
        p2 /= p1]
  where
    usage = Map.assocs df
    free = sortBy (comparing (Down . snd))
        [(pos, available u) | (pos, u) <- usage]

solve1 :: Input -> Int
solve1 = length . viable_pairs

-- Part Two --

{-
The actual problem is much easier than stated.  As hinted in the example,
there are three kinds of nodes:
- exactly one node with usage 0 (the hole)
- nodes with capacity 85-94 and usage 64-73, i.e. all interchangeable
- nodes with capacity 501-510 and usage 490-498, i.e. blocked

The question thus becomes a 15-puzzle, using the hole to move the goal.
-}

data State = State { goal :: Position, hole :: Position }
  deriving (Show, Eq, Ord)

initState :: Input -> State
initState df = State { goal = Position max_x 0, hole = first_hole }
  where
    max_x = maximum [x | Position x _ <- Map.keys df]
    first_hole = head [p | (p, Usage _ 0) <- Map.assocs df]

-- ways we can move the hole (may also move the goal into the old hole)
moves :: Input -> State -> [State]
moves df (State g h) =
    [State (if g == n then h else g) n |
        n <- neighbours h, open df n,
        -- don't move the hole away once it has found the goal
        -- (makes the number of states linear in the number of positions)
        not (near h g) || near n g]

near :: Position -> Position -> Bool
near (Position x1 y1) (Position x2 y2) = abs (x1-x2) <= 1 && abs (y1-y2) <= 1

neighbours :: Position -> [Position]
neighbours p = map (p .+.) unitVectors

finished :: State -> Bool
finished s = goal s == zero

-- a position we could move the hole to
open :: Input -> Position -> Bool
open df pos = case Map.lookup pos df of
    Nothing -> False
    Just (Usage _ u) -> u < 100

solve2 :: Input -> Int
solve2 df =
    length $ takeWhile (not . any finished) $ bfs (moves df) $ [initState df]

testInput :: String
testInput =
    "root@ebhq-gridcenter# df -h\n\
    \Filesystem            Size  Used  Avail  Use%\n\
    \/dev/grid/node-x0-y0   10T    8T     2T   80%\n\
    \/dev/grid/node-x0-y1   11T    6T     5T   54%\n\
    \/dev/grid/node-x0-y2   32T   28T     4T   87%\n\
    \/dev/grid/node-x1-y0    9T    7T     2T   77%\n\
    \/dev/grid/node-x1-y1    8T    0T     8T    0%\n\
    \/dev/grid/node-x1-y2   11T    7T     4T   63%\n\
    \/dev/grid/node-x2-y0   10T    6T     4T   60%\n\
    \/dev/grid/node-x2-y1    9T    8T     1T   88%\n\
    \/dev/grid/node-x2-y2    9T    6T     3T   66%\n"

tests2 :: [(String, Int)]
tests2 = [(testInput, 7)]

main :: IO ()
main = do
    s <- readFile "input/22.txt"
    let input = parse s
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
