module Main where

import Utilities
import Cartesian
import Intcode
import Data.Char
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = Memory

parse :: String -> Input
parse = readMemory

-- Part One

type Scaffold = Set Position

data Direction = Up | Dn | Lt | Rt
    deriving (Bounded, Enum, Show)

move :: Direction -> Position
move Up = Position 0 (-1)
move Dn = Position 0 1
move Lt = Position (-1) 0
move Rt = Position 1 0

dirNames :: String
dirNames = "^v<>"

data State = State {
    scaffold :: Set Position,
    botPosition :: Position,
    botDirection :: Maybe Direction
    }
    deriving Show

scanImage :: String -> State
scanImage s = State {
    scaffold = Set.fromList [p | (p, c) <- pcs, c `elem` "#^v<>"],
    botPosition = bp,
    botDirection = mb_dir
    }
  where
    pcs = readGrid s
    (bp, bc) = head [(p, c) | (p, c) <- pcs, c `elem` "X^v<>"]
    mb_dir = lookup bc (zip "^v<>" allValues)

intersections :: State -> [Position]
intersections s = filter crossing (Set.toList m)
  where
    m = scaffold s
    crossing p = Set.fromList [p .+. move d | d <- allValues] `Set.isSubsetOf` m

alignment :: Position -> Int
alignment (Position x y) = x*y

getImage :: Memory -> String
getImage mem = map fromValue (streamFunction mem [])

part1 :: String -> Int
part1 = sum . map alignment . intersections . scanImage

solve1 :: Input -> Int
solve1 = part1 . getImage

testInput :: String
testInput =
    "..#..........\n\
    \..#..........\n\
    \#######...###\n\
    \#.#...#...#.#\n\
    \#############\n\
    \..#...#...#..\n\
    \..#####...^..\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 76)]

-- Part Two

data Turn = L | R
    deriving (Bounded, Enum, Eq, Show)

turn :: Turn -> Direction -> Direction
turn L Rt = Up
turn L Dn = Rt
turn L Lt = Dn
turn L Up = Lt
turn R Up = Rt
turn R Rt = Dn
turn R Dn = Lt
turn R Lt = Up

-- Turn and move a number of positions
type Segment = (Turn, Int)

-- The "scaffold" is a single path with the robot at one end facing sideways,
-- so it can be reduced to a list of segments.
segments :: State -> [Segment]
segments (State s p (Just d)) = segmentsFrom s p d
segments (State _ _ Nothing) = error "No bot"

segmentsFrom :: Set Position -> Position -> Direction -> [Segment]
segmentsFrom s p d = case tds of
    [] -> []
    [(t, d')] ->
        let ps = takeWhile (`Set.member` s) (iterate (.+. move d') p) in
        (t, length ps - 1):segmentsFrom s (last ps) d'
    _ -> error "More than one turn"
  where
    tds = [(t, d') |
        t <- allValues, let d' = turn t d, (p .+. move d') `Set.member` s]

-- A two-level program for traversing the path
data Plan = Plan {
    mainRoutine :: [Int], -- all 0, 1 or 2, in reverse order
    functions :: [[Segment]]
    }
    deriving Show

showMain :: [Int] -> String
showMain ns = intersperse ',' [chr (ord 'A' + n) | n <- reverse ns]

showFunction :: [Segment] -> String
showFunction ts =
    concat (intersperse "," [show t ++ "," ++ show n | (t, n) <- ts])

showPlan :: Plan -> [String]
showPlan (Plan m fs) = showMain m : map showFunction fs

validFunction :: [Segment] -> Bool
validFunction ts = length (showFunction ts) <= 20

plans :: [Segment] -> [Plan]
plans = extendPlan (Plan [] [])

-- ways of extending a plan for the current point to the rest of the segments
extendPlan :: Plan -> [Segment] -> [Plan]
extendPlan plan [] = [plan]
extendPlan (Plan ns fs) ts
  | length ns == 10 = [] -- main routine is full
  | otherwise =
    [plan | -- try one of the movement functions we have already
        (n, f) <- zip [0..] fs,
        f `isPrefixOf` ts, let ts' = drop (length f) ts,
        plan <- extendPlan (Plan (n:ns) fs) ts'] ++
    [plan | -- use a prefix of the remaining input as a new movement function
        length fs < 3,
        (f, ts') <- takeWhile (validFunction . fst) (tail (splits ts)),
        plan <- extendPlan (Plan (length fs:ns) (fs ++ [f])) ts']

imagePlans :: String -> [Plan]
imagePlans = plans . segments . scanImage

solve2 :: Input -> Int
solve2 mem = fromValue (last (streamFunction bot cmds))
  where
    bot = setMemory 0 2 mem
    plan = head (imagePlans (getImage mem))
    cmds = map toValue (unlines (showPlan plan ++ ["n"]))

testInput2 :: String
testInput2 =
    "#######...#####\n\
    \#.....#...#...#\n\
    \#.....#...#...#\n\
    \......#...#...#\n\
    \......#...###.#\n\
    \......#.....#.#\n\
    \^########...#.#\n\
    \......#.#...#.#\n\
    \......#########\n\
    \........#...#..\n\
    \....#########..\n\
    \....#...#......\n\
    \....#...#......\n\
    \....#...#......\n\
    \....#####......\n"

tests2 :: [(String, [[String]])]
tests2 = [(testInput2, [
    ["A,A,B,B,C,B,B,A,A,C", "R,8", "R,4", "R,8,L,6,L,2"],
    ["A,A,B,A,C,B,A,A,A,C", "R,8", "R,4,R,4", "L,6,L,2"],
    ["A,A,B,C,B,A,A,C", "R,8", "R,4,R,4", "R,8,L,6,L,2"],
    ["A,A,B,C,B,A,A,C", "R,8", "R,4,R,4,R,8", "L,6,L,2"],
    ["A,B,B,C,B,B,A,C", "R,8,R,8", "R,4", "R,8,L,6,L,2"],
    ["A,B,C,B,A,C", "R,8,R,8", "R,4,R,4", "R,8,L,6,L,2"],
    ["A,B,C,B,A,C", "R,8,R,8", "R,4,R,4,R,8", "L,6,L,2"],
    ["A,B,C,B", "R,8,R,8,R,4,R,4", "R,8,L,6,L,2", "R,4,R,4,R,8,R,8"],
    ["A,B,C", "R,8,R,8,R,4,R,4", "R,8,L,6,L,2,R,4,R,4", "R,8,R,8,R,8,L,6,L,2"],
    ["A,B,C,B", "R,8,R,8,R,4,R,4,R,8", "L,6,L,2", "R,4,R,4,R,8,R,8,R,8"],
    ["A,B,C", "R,8,R,8,R,4,R,4,R,8", "L,6,L,2,R,4,R,4", "R,8,R,8,R,8,L,6,L,2"],
    ["A,B,C", "R,8,R,8,R,4,R,4,R,8", "L,6,L,2,R,4,R,4,R,8", "R,8,R,8,L,6,L,2"]])]

main :: IO ()
main = do
    s <- readFile "input/17.txt"
    let input = parse s
    putStr (unlines (failures "part1" part1 tests1))
    print (solve1 input)
    putStr (unlines (failures "imagePlans" (map showPlan . imagePlans) tests2))
    print (solve2 input)
