module Main where

import Utilities
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Object a = Gen a | Chip a
  deriving (Show, Eq, Ord)
type Input = [[Object String]]

generators :: [Object a] -> [a]
generators os = [e | Gen e <- os]

microchips :: [Object a] -> [a]
microchips os = [e | Chip e <- os]

type FloorNo = Int -- 0..3

topFloor :: FloorNo
topFloor = 3

data State = State { elevator :: FloorNo, ordered_gcs :: [(FloorNo, FloorNo)] }
  deriving (Show, Eq, Ord)

showState :: State -> String
showState (State elev gcs) = unlines [
    "F" ++ show (f+1) ++ (if f == elev then " E " else " . ") ++
        showFloor f gcs |
    f <- reverse [0..topFloor]]

showFloor :: FloorNo -> [(FloorNo, FloorNo)] -> String
showFloor f gcs = unwords [
    (if f == g then "G" else ".") ++ (if f == c then "C" else ".") |
    (g, c) <- gcs]

safeState :: State -> Bool
safeState s = Set.null (chipFloors s `Set.intersection` genFloors s)

-- floors with unguarded chips
chipFloors :: State -> Set FloorNo
chipFloors (State _ gcs) = Set.fromList [cf | (gf, cf) <- gcs, gf /= cf]

-- floors with generators
genFloors :: State -> Set FloorNo
genFloors (State _ gcs) = Set.fromList (map fst gcs)

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

-- possible moves from state

moves :: State -> [State]
moves s = filter safeState
    [moveTo f os s | os <- chooseBetween 1 2 (movable s), f <- adjacentFloors s]

movable :: State -> [Object Element]
movable (State elev gcs) =
    [Gen p | (p, (g, _)) <- pgcs, g == elev] ++
    [Chip p | (p, (_, c)) <- pgcs, c == elev]
  where
    pgcs = zip [(0::Element)..] gcs

adjacentFloors :: State -> [FloorNo]
adjacentFloors (State elev _) =
    [elev-1 | elev > 0] ++ [elev+1 | elev < topFloor]

moveTo :: FloorNo -> [Object Element] -> State -> State
moveTo f os (State _ gcs) =
    State f (sort (map move (zip [(0::Element)..] gcs)))
  where
    move (p, (g, c)) =
        (if Gen p `elem` os then f else g,
         if Chip p `elem` os then f else c)

solve :: Input -> Int
solve rs = length $ takeWhile (not . any finished) $ bfs moves $ [mkState rs]

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

-- Part Two --

initInput2 :: Input
initInput2 = (extras++floor1):rest
  where
    floor1:rest = initInput
    -- An elerium generator.
    -- An elerium-compatible microchip.
    -- A dilithium generator.
    -- A dilithium-compatible microchip
    extras = [Gen "elerium", Chip "elerium", Gen "dilithium", Chip "dilithium"]

main :: IO ()
main = do
    print (solve initInput)
    print (solve initInput2)
