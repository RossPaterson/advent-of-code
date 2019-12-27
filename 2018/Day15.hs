module Main where

import Utilities
import Graph
import Data.List
import Data.Maybe
import Data.Ord
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = (Cavern, Units)

type Cavern = Set Position
data Position = Pos { column :: Int, row :: Int }
  deriving (Show, Eq)

-- reading order
instance Ord Position where
    compare (Pos x1 y1) (Pos x2 y2) = compare y1 y2 <> compare x1 x2

type Units = Map Position (Unit, Int)
data Unit = Goblin | Elf
  deriving (Show, Eq, Ord)

parse :: String -> Input
parse s = (cavern, units)
  where
    pcs = [(Pos x y, c) | (y, l) <- zip [0..] (lines s), (x, c) <- zip [0..] l]
    cavern = Set.fromList [p | (p, c) <- pcs, c /= '#']
    units = Map.fromList [(p, (u, initHP)) | (p, c) <- pcs, u <- unit c]
    unit 'G' = [Goblin]
    unit 'E' = [Elf]
    unit _ = []

initHP :: Int
initHP = 200

-- Part One

solve1 :: Input -> Int
solve1 = outcome . uncurry (battle (const 3))

-- The outcome of a complete battle
data Result = Result { full_rounds :: Int, final_units :: Units }
  deriving Show

-- Hash of the outcome of a battle, for puzzle answers
outcome :: Result -> Int
outcome (Result n units) = n * snd (head (summary units))

summary :: Units -> [(Unit, Int)]
summary units =
    [(fst (head us), sum (map snd us)) |
        us <- groupBy (same fst) (sortBy (comparing fst) (Map.elems units))]

-- Run a complete battle
battle :: (Unit -> Int) -> Cavern -> Units -> Result
battle attackPower cavern units = Result n units'
  where
    units' = whileRight (fullRound attackPower cavern) units
    n = length (iterateWhileRight (fullRound attackPower cavern) units) - 1

-- Give each units a turn in reading order.
-- Returns the updated units with tagged with Left if the battle
-- ended during this turn and Right if not.
fullRound :: (Unit -> Int) -> Cavern -> Units -> Either Units Units
fullRound attackPower cavern units =
    unitTurns attackPower cavern units Set.empty us
  where
    us = sort (Map.keys units)

-- Give the units at positions ps a turn in order.
-- We need to record units that die during this turn and ignore them if
-- we come to them later in the list.
unitTurns ::
    (Unit -> Int) -> Cavern -> Units -> Set Position -> [Position] ->
    Either Units Units
unitTurns _ _ units _ [] = Right units
unitTurns attackPower cavern units dead (p:ps)
  | Set.member p dead = unitTurns attackPower cavern units dead ps
  | otherwise = case unitTurn attackPower cavern units dead p of
    Nothing -> Left units
    Just (units', mb_killed) ->
        let dead' = maybe id Set.insert mb_killed dead in
        unitTurns attackPower cavern units' dead' ps

-- Let the unit at p (which is still alive) have a turn.
-- Returns Nothing if there are no more enemies of this unit.
-- Otherwise returns the updated units and the position of a unit killed
-- on this turn (if any).
unitTurn :: (Unit -> Int) -> Cavern -> Units -> Set Position -> Position ->
    Maybe (Units, Maybe Position)
unitTurn attackPower cavern units _ p
  | null targets = Nothing
  | otherwise = Just $ case attackee units p of
        Just ap -> attack ap apower units
        Nothing -> case bestMove open startPoints attackPoints of
            Nothing -> (units, Nothing) -- cannot move
            Just p' ->
                let units' = Map.insert p' u (Map.delete p units) in
                case attackee units' p' of
                    Just ap' -> attack ap' apower units'
                    Nothing -> (units', Nothing)
  where
    u = units!p
    utype = fst u
    apower = attackPower utype
    open = Set.difference cavern (Map.keysSet units)
    startPoints = filterMembers open (neighbours p)
    targets = [pe | (pe, (ue, _)) <- Map.toList units, ue /= utype]
    attackPoints =
        open `Set.intersection`
        Set.fromList [pa | pe <- targets, pa <- neighbours pe]

-- attack the unit at position p
attack :: Position -> Int -> Units -> (Units, Maybe Position)
attack p apower units
  | hp' <= 0 = (Map.delete p units, Just p)
  | otherwise = (Map.insert p (u, hp') units, Nothing)
  where
    (u, hp) = units!p
    hp' = hp - apower

-- which position to attack from p, if any
-- If more than one is possible, choose the first in reading order.
attackee :: Units -> Position -> Maybe Position
attackee units p
  | null inRange = Nothing
  | otherwise =
    listToMaybe $ map fst $ sortBy (comparing snd <> comparing fst) inRange
  where
    u = fst (units!p)
    inRange = [(np, hp) |
        np <- neighbours p,
        (u', hp) <- maybeToList (Map.lookup np units),
        u' /= u]

-- If any move towards a target is possible, return a move towards the
-- closest target.  Break ties by choosing the first target in reading
-- order, and then the first step in reading order.
bestMove :: Set Position -> [Position] -> Set Position -> Maybe Position
bestMove open starts targets =
   listToMaybe $ map fst $
   sortBy (comparing (fst . snd) <> comparing (snd . snd) <> comparing fst) $
   [(start, (dist, target)) |
       start <- starts,
       (dist, close_targets) <- maybeToList (closest open start targets),
       target <- close_targets]

-- If there is a path through open to any member of targets, returns
-- Just (n, ps) where n is the length of the shortest such path and ps
-- are the elements of targets reachable in n steps.
-- If there is no such path, returns Nothing.
closest :: Set Position -> Position -> Set Position -> Maybe (Int, [Position])
closest open start targets =
    case span null found of
        (_, []) -> Nothing
        (misses, hits:_) -> Just (length misses, hits)
  where
    found =
        map (filterMembers targets) $
            bfs (filterMembers open . neighbours) [start]

-- filter members of the set
filterMembers :: Ord a => Set a -> [a] -> [a]
filterMembers s = filter (flip Set.member s)

-- neighbouring points of p in reading order
neighbours :: Position -> [Position]
neighbours (Pos x y) = [Pos x (y-1), Pos (x-1) y, Pos (x+1) y, Pos x (y+1)]

-- Output for debugging purposes

-- String presentation of the result of a battle
showResult :: Cavern -> Result -> String
showResult cavern (Result n units) =
    showState cavern units ++
    unlines ["", "Combat ends after " ++ show n ++ " full rounds",
        show (Map.size units) ++ " " ++ show winner ++ "s win with " ++
            show hp ++ " total hit points left",
        "Outcome: " ++ show n ++ " * " ++ show hp ++ " = " ++ show (n*hp), ""]
  where
    (winner, hp) = head (summary units)

-- String representation of the grid
showState :: Cavern -> Units -> String
showState cavern units = unlines [showLine y | y <- [0..ymax]]
  where
    xmax = maximum (map column (Set.toList cavern)) + 1
    ymax = maximum (map row (Set.toList cavern)) + 1
    showLine y =
        map showPos ps ++ "    " ++
        intercalate ", " [showUnit u : "(" ++ show hp ++ ")" |
            p <- ps, (u, hp) <- maybeToList (Map.lookup p units)]
      where
        ps = [Pos x y | x <- [0..xmax]]
    showPos p
      | not (Set.member p cavern) = '#'
      | otherwise = maybe '.' (showUnit . fst) (Map.lookup p units)
    showUnit u = head (show u)

tests1 :: [(String, Int)]
tests1 =
    [(example4, 27730), (example5, 36334), (example6, 39514),
     (example7, 27755), (example8, 28944), (example9, 18740)]

example1 :: String
example1 = "\
\#######\n\
\#E..G.#\n\
\#...#.#\n\
\#.G.#G#\n\
\#######\n"

example2 :: String
example2 = "\
\#######\n\
\#.E...#\n\
\#.....#\n\
\#...G.#\n\
\#######\n"

example3 :: String
example3 = "\
\#########\n\
\#G..G..G#\n\
\#.......#\n\
\#.......#\n\
\#G..E..G#\n\
\#.......#\n\
\#.......#\n\
\#G..G..G#\n\
\#########\n"

example4 :: String
example4 = "\
\#######\n\
\#.G...#\n\
\#...EG#\n\
\#.#.#G#\n\
\#..G#E#\n\
\#.....#\n\
\#######\n"

example5 :: String
example5 = "\
\#######\n\
\#G..#E#\n\
\#E#E.E#\n\
\#G.##.#\n\
\#...#E#\n\
\#...E.#\n\
\#######\n"

example6 :: String
example6 = "\
\#######\n\
\#E..EG#\n\
\#.#G.E#\n\
\#E.##E#\n\
\#G..#.#\n\
\#..E#.#\n\
\#######\n"

example7 :: String
example7 = "\
\#######\n\
\#E.G#.#\n\
\#.#G..#\n\
\#G.#.G#\n\
\#G..#.#\n\
\#...E.#\n\
\#######\n"

example8 :: String
example8 = "\
\#######\n\
\#.E...#\n\
\#.#..G#\n\
\#.###.#\n\
\#E#G#G#\n\
\#...#G#\n\
\#######\n"

example9 :: String
example9 = "\
\#########\n\
\#G......#\n\
\#.E.#...#\n\
\#..##..G#\n\
\#...##..#\n\
\#...#...#\n\
\#.G...G.#\n\
\#.....G.#\n\
\#########\n"

-- Part Two

-- Report the battle outcome with elf attack power increased to the
-- minimum that will allow all elves to survive.
-- (It is not necessarily the case that greater power will still have
-- this property, so we must search linearly.)
solve2 :: Input -> Int
solve2 (cavern, units) =
    outcome $ head
        [r | k <- [4..],
            let r = battle (adjustAttackPower k) cavern units,
            numElves (final_units r) == n]
  where
    n = numElves units

numElves :: Units -> Int
numElves units = length [u | (u, _hp) <- Map.elems units, u == Elf]

adjustAttackPower :: Int -> Unit -> Int
adjustAttackPower _ Goblin = 3
adjustAttackPower n Elf = n

tests2 :: [(String, Int)]
tests2 =
    [(example4, 4988), (example6, 31284), (example7, 3478),
     (example8, 6474), (example9, 1140)]

fullTest :: (Cavern, Units) -> String
fullTest (cavern, units) =
    unlines ["After " ++ show n ++ " rounds:\n" ++ showState cavern s |
        (n, s) <- zip [0..] (iterateWhileRight (fullRound (const 3) cavern) units)]

test :: (Cavern, Units) -> String
test (cavern, units) = showResult cavern (battle (const 3) cavern units)

main :: IO ()
main = do
    s <- readFile "input/15.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
