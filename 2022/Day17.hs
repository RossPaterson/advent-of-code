module Main where

import Utilities
import Geometry
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = [Direction]
data Direction = L | R
    deriving (Eq, Ord, Show)

parse :: String -> Input
parse = map jet . head . lines
  where
    jet '<' = L
    jet '>' = R
    jet _ = error "bad character"

type Block = Set Point2

parseBlock :: String -> Block
parseBlock s =
    Set.fromList [Point2 x y |
        (y, line) <- zip [0..] (reverse (lines s)),
        (x, c) <- zip [0..] line, c == '#']

blocks :: [Block]
blocks = map parseBlock [
    "####\n",
    ".#.\n\
    \###\n\
    \.#.\n",
    "..#\n\
    \..#\n\
    \###\n",
    "#\n\
    \#\n\
    \#\n\
    \#\n",
    "##\n\
    \##\n"]

-- Part One

type Tower = Set Point2

height :: Tower -> Int
height ps = maximum (0:[y | Point2 _ y <- Set.elems ps])

shiftBlock :: Point2 -> Block -> Set Point2
shiftBlock p = Set.mapMonotonic (p .+.)

width :: Int
width = 7

-- attempt to push a block sideways
push :: Direction -> Tower -> Block -> Point2 -> Point2
push L ps b offset
  | x > 0 && Set.disjoint placed_b ps = offset'
  | otherwise = offset
  where
    offset'@(Point2 x _) = offset .+. Point2 (-1) 0
    placed_b = shiftBlock offset' b
push R ps b offset
  | max_x <= width && Set.disjoint placed_b ps = offset'
  | otherwise = offset
  where
    max_x = maximum [x | Point2 x _ <- Set.elems placed_b]
    offset' = offset .+. Point2 1 0
    placed_b = shiftBlock offset' b

fall :: Tower -> Block -> Point2 -> Maybe Point2
fall ps b offset
  | y > 0 && Set.disjoint placed_b ps = Just offset'
  | otherwise = Nothing
  where
    offset'@(Point2 _ y) = offset .+. Point2 0 (-1)
    placed_b = shiftBlock offset' b

-- Adding one block to the cave

data State = State {
    tower :: Tower,
    block :: Block,
    block_pos :: Point2
    }
    deriving (Show)

showState :: State -> String
showState s = unlines (linesState s)

showShortState :: State -> String
showShortState s = unlines (take 20 (linesState s))

linesState :: State -> [String]
linesState s = ([row y | y <- [max_y, max_y-1..1]] ++ ["+-------+"])
  where
    placed_block = placedBlock s
    max_y = height (Set.union placed_block (tower s))
    row y = "|" ++ [char (Point2 x y) | x <- [1..7]] ++ "|"
    char p
      | Set.member p placed_block = '@'
      | Set.member p (tower s) = '#'
      | otherwise = '.'

placedBlock :: State -> Set Point2
placedBlock s = shiftBlock (block_pos s) (block s)

-- apply one move to the block
move :: State -> Direction -> (State, Bool)
move s d = (s { block_pos = final_offset }, at_rest)
  where
    pushed_offset = push d (tower s) (block s) (block_pos s)
    (final_offset, at_rest) = case fall (tower s) (block s) pushed_offset of
        Just offset' -> (offset', False)
        Nothing -> (pushed_offset, True)

-- Infinite list of directions, keeping track of how many we used for
-- tracing purposes.
data Jets = Jets { used :: !Int, directions :: [Direction] }
    deriving (Show)

makeJets :: [Direction] -> Jets
makeJets ds = Jets 0 (cycle ds)

getDirection :: Jets -> (Direction, Jets)
getDirection (Jets n (d:ds)) = (d, Jets (n+1) ds)
getDirection _ = error "ran out of directions"

-- apply moves to the block until it comes to rest
moves :: State -> Jets -> (Set Point2, Jets)
moves s jets
  | at_rest = (Set.union (placedBlock s') (tower s'), jets')
  | otherwise = moves s' jets'
  where
    (d, jets') = getDirection jets
    (s', at_rest) = move s d

states :: [Direction] -> [(State, Int)]
states dirs = allStates Set.empty (cycle blocks) (makeJets dirs)

allStates :: Set Point2 -> [Block] -> Jets -> [(State, Int)]
allStates _ [] _ = error "no blocks"
allStates ps (b:bs) jets = (s, used jets):case moves s jets of
    (ps', jets') -> allStates ps' bs jets'
  where
    s = addBlock b ps

addBlock :: Block -> Set Point2 -> State
addBlock b ps = State {
    tower = ps,
    block = b,
    block_pos = newPosition ps
    }

-- position at which a new block is created
newPosition :: Tower -> Point2
newPosition ps = Point2 3 (height ps + 4)

-- height of the tower after dropping n blocks
heightAfter :: Int -> Input -> Int
heightAfter n = height . tower . head . drop n . map fst . states

solve1 :: Input -> Int
solve1 = heightAfter 2022

testInput :: String
testInput = ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"

tests1 :: [(String, Int)]
tests1 = [(testInput, 3068)]

-- Part Two

trace :: Input -> [String]
trace dirs =
    [show n ++ " blocks: block " ++ show (n `mod` nblocks) ++
        ", jet " ++ show (dir_count `mod` ndirs) ++
        ", height = " ++ show (height (tower s)) |
        (n, (s, dir_count)) <- zip [0..] (states dirs)]
  where
    nblocks = length blocks
    ndirs = length dirs

steps :: Int
steps = 1000000000000

-- Assuming the pattern repeats after the given number of block cycles,
-- give the height after steps blocks.
extrapolate :: Int -> Input -> Int
extrapolate block_period ds = (steps `div` major_period - 1)*(h2 - h1) + h1
  where
    major_period = block_period * length blocks
    -- give it more than one major period to stabilize
    n1 = (steps `mod` major_period) + major_period
    n2 = n1 + major_period
    h1 = heightAfter n1 ds
    h2 = heightAfter n2 ds

-- For the test input, 7 cycles through the blocks corresponds to
-- 5 cycles through the jets.  (Discovered by inspecting the trace)
test_block_period :: Int
test_block_period = 7

tests2 :: [(String, Int)]
tests2 = [(testInput, 1514285714288)]

-- For my input, after a while this many cycles through the blocks
-- (discovered by inspection) corresponds to a single cycle through the
-- jets, and thence the height of the tower grows linearly with each
-- major cycle.
input_block_period :: Int
input_block_period = 347

solve2 :: Input -> Int
solve2 = extrapolate input_block_period

main :: IO ()
main = do
    s <- readFile "input/17.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2"
        (extrapolate test_block_period . parse) tests2))
    print (solve2 input)
