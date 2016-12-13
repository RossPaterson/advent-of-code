module Day11 where

import Utilities
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Hashable
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Graph.AStar

data Object = Gen String | Chip String
  deriving (Show, Eq, Ord)
type Input = [[Object]]

generators :: [Object] -> [String]
generators os = [e | Gen e <- os]

microchips :: [Object] -> [String]
microchips os = [e | Chip e <- os]

type FloorNo = Int -- 0..3
data State = State { elevator :: FloorNo, ordered_gcs :: [(FloorNo, FloorNo)] }
  deriving (Show, Eq, Ord)

topFloor :: FloorNo
topFloor = 3

instance Hashable State where
    hashWithSalt salt (State elev gcs) = salt + foldr shift elev gcs
      where
        shift (g, c) h = h*16 + g*4 + c

showState :: State -> String
showState (State elev gcs) = unlines [
    "F" ++ show (f+1) ++ (if f == elev then " E " else " . ") ++
        showFloor f gcs |
    f <- reverse [0..topFloor]]

showFloor :: FloorNo -> [(FloorNo, FloorNo)] -> String
showFloor f gcs = unwords [
    (if f == g then "G" else ".") ++ (if f == c then "C" else ".") |
    (g, c) <- gcs]

-- finished if all objects on top floor
-- (uses the fact that pairs are ordered)
finished :: State -> Bool
finished (State _ (gc:gcs)) = gc == (topFloor, topFloor)
finished (State _ []) = True

mkState :: Input -> State
mkState floors =
    State 0 (toFloors (map generators floors) (map microchips floors))

toFloors :: Ord a => [[a]] -> [[a]] -> [(FloorNo, FloorNo)]
toFloors gss css = sort (Map.elems (Map.intersectionWith (,) gens chips))
  where
    gens = Map.fromList [(g, f) | (f, gs) <- zip [0..] gss, g <- gs]
    chips = Map.fromList [(c, f) | (f, cs) <- zip [0..] css, c <- cs]

type Element = Int -- index of gcs list of a state
type Floor = ([Element], [Element]) -- (gens, chips)

sizeFloor :: Floor -> Int
sizeFloor (gens, chips) = length gens + length chips

-- list of objects on each floor
elementsToFloors :: [(FloorNo, FloorNo)] -> [Floor]
elementsToFloors gcs =
    [([e | (e, (g, _)) <- egcs, g == f], [e | (e, (_, c)) <- egcs, c == f]) |
        f <- [0..topFloor]]
  where
    egcs = zip [0..] gcs

floorsToElements :: [Floor] -> [(FloorNo, FloorNo)]
floorsToElements floors = toFloors (map fst floors) (map snd floors)

safeFloor :: Floor -> Bool
safeFloor (gens, chips) = null gens || null (chips \\ gens)

-- lower bound on number of steps to finish
heuristic :: State -> Int
heuristic (State _ gcs) =
    sum [max 1 (2*n-3) | n <- scanl1 (+) (init counts)]
  where
    counts = map sizeFloor (elementsToFloors gcs)

-- possible moves from state

moves :: State -> [State]
moves (State elev gcs) =
    uniq $ sort [State elev' (floorsToElements floors') |
        (carry, rest) <- choose12 this, safeFloor rest,
        (elev', floors') <-
           [(elev+1, below ++ rest:above') | above' <- move_to carry above] ++
           [(elev-1, below' ++ rest:above) |
               below' <- map reverse (move_to carry (reverse below))]]
  where
    (below, this:above) = splitAt elev (elementsToFloors gcs)

move_to :: Floor -> [Floor] -> [[Floor]]
move_to (gs1, cs1) ((gs2, cs2):rest)
  | safeFloor next = [next:rest]
  where
    next = (gs1 ++ gs2, cs1 ++ cs2)
move_to _ _ = []

-- choose 1 or 2 objects from a floor
choose12 :: Floor -> [(Floor, Floor)]
choose12 (gs, cs) =
    [((gs_sel, cs_sel), (gs_rest, cs_rest)) |
        (gs_sel, gs_rest) <- choose 1 gs,
        (cs_sel, cs_rest) <- choose 1 cs] ++
    [((gs_sel, []), (gs_rest, cs)) |
        n <- [1, 2], (gs_sel, gs_rest) <- choose n gs] ++
    [(([], cs_sel), (gs, cs_rest)) |
        n <- [1, 2], (cs_sel, cs_rest) <- choose n cs]

solve :: Input -> Int
solve = length . fromJust .
    aStar (HashSet.fromList . moves) (const (const 1)) heuristic finished .
    mkState

testInput :: Input
testInput = [
    -- The first floor contains a hydrogen-compatible microchip and
    -- a lithium-compatible microchip.
    [Chip "hydrogen", Chip "lithium"],
    -- The second floor contains a hydrogen generator.
    [Gen "hydrogen"],
    -- The third floor contains a lithium generator.
    [Gen "lithium"],
    -- The fourth floor contains nothing relevant.
    []]

initInput :: Input
initInput = [
    -- The first floor contains a thulium generator, a thulium-compatible
    -- microchip, a plutonium generator, and a strontium generator.
    [Gen "thulium", Chip "thulium", Gen "plutonium", Gen "strontium"],
    -- The second floor contains a plutonium-compatible microchip and a
    --  strontium-compatible microchip.
    [Chip "plutonium", Chip "strontium"],
    -- The third floor contains a promethium generator, 
    -- a promethium-compatible microchip, a ruthenium generator, and 
    -- a ruthenium-compatible microchip.
    [Gen "promethium", Chip "promethium", Gen "ruthenium", Chip "ruthenium"],
    -- The fourth floor contains nothing relevant.
    []]

puzzle1 :: IO ()
puzzle1 = print (solve initInput)

initInput2 :: Input
initInput2 = (extras++floor1):rest
  where
    floor1:rest = initInput
    -- An elerium generator.
    -- An elerium-compatible microchip.
    -- A dilithium generator.
    -- A dilithium-compatible microchip
    extras = [Gen "elerium", Chip "elerium", Gen "dilithium", Chip "dilithium"]

puzzle2 :: IO ()
puzzle2 = print (solve initInput2)
