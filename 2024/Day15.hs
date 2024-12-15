module Main where

import Geometry
import Utilities
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = (Set Position, Set Position, Position, [Direction])

data Direction = R | D | L | U
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

parse :: String -> Input
parse s = case paragraphs s of
    [warehouse, moves] ->
        let
            pcs = readGrid warehouse
            free_space = Set.fromList [p | (p, c) <- pcs, c /= '#']
            boxes = Set.fromList [p | (p, c) <- pcs, c == 'O']
            start = head [p | (p, c) <- pcs, c == '@']
            dirs = map direction (concat (lines moves))
        in (free_space, boxes, start, dirs)
    _ -> error "bad input"

direction :: Char -> Direction
direction '<' = L
direction '>' = R
direction '^' = U
direction 'v' = D
direction _ = error "bad direction"

-- Part One

oneStep :: Direction -> Position
oneStep U = Position 0 (-1)
oneStep R = Position 1 0
oneStep D = Position 0 1
oneStep L = Position (-1) 0

type State = (Position, Set Position)

solve1 :: Input -> Int
solve1 (free_space, boxes, start, dirs) =
    measure $ snd $ foldl (move free_space) (start, boxes) dirs

-- try to move in the given direction, pushing any boxes that are in the way
move :: Set Position -> State -> Direction -> State
move free_space (pos, boxes) dir
  | Set.member pos' free_space && Set.isSubsetOf load' free_space =
    (pos', boxes')
  | otherwise = (pos, boxes)
  where
    dp = oneStep dir
    pos' = pos .+. dp
    load = boxesInTheWay pos boxes dir
    load' = Set.mapMonotonic (.+. dp) load
    boxes' = (boxes `Set.difference` load) `Set.union` load'

-- boxes that are in the way of a move in the given direction
boxesInTheWay :: Position -> Set Position -> Direction -> Set Position
boxesInTheWay pos boxes dir =
    Set.fromList (takeWhile (flip Set.member boxes) (iterate (.+. dp) pos'))
  where
    dp = oneStep dir
    pos' = pos .+. dp

measure :: Set Position -> Int
measure ps = sum [x + 100*y | Position x y <- Set.elems ps]

testInputSmall :: String
testInputSmall = "\
    \########\n\
    \#..O.O.#\n\
    \##@.O..#\n\
    \#...O..#\n\
    \#.#.O..#\n\
    \#...O..#\n\
    \#......#\n\
    \########\n\
    \\n\
    \<^^>>>vv<v>>v<<\n\
    \"

testInputLarge :: String
testInputLarge = "\
    \##########\n\
    \#..O..O.O#\n\
    \#......O.#\n\
    \#.OO..O.O#\n\
    \#..O@..O.#\n\
    \#O#..O...#\n\
    \#O..O..O.#\n\
    \#.OO.O.OO#\n\
    \#....O...#\n\
    \##########\n\
    \\n\
    \<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^\n\
    \vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v\n\
    \><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<\n\
    \<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^\n\
    \^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><\n\
    \^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^\n\
    \>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^\n\
    \<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>\n\
    \^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>\n\
    \v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInputSmall, 2028), (testInputLarge, 10092)]

-- Part Two

-- Make the space and boxes twice as wide
scaleUp :: Input -> Input
scaleUp (free_space, boxes, start, dirs) =
    (free_space', Set.mapMonotonic stretch boxes, stretch start, dirs)
  where
    free_space' =
       Set.fromList [stretch p .+. dp | p <- Set.elems free_space,
           dp <- [Position 0 0, Position 1 0]]

stretch :: Position -> Position
stretch (Position x y) = Position (2*x) y

-- Now a box occupies pos and pos .+. Position 1 0

solve2 :: Input -> Int
solve2 input = case scaleUp input of
    (free_space, boxes, start, dirs) ->
        measure $ snd $ foldl (move2 free_space) (start, boxes) dirs

-- try to move in the given direction, pushing any double-width boxes
-- that are in the way
move2 :: Set Position -> State -> Direction -> State
move2 free_space (pos, boxes) dir
  | Set.member pos' free_space &&
    Set.isSubsetOf load' free_space &&
    Set.isSubsetOf (Set.mapMonotonic (.+. Position 1 0) load') free_space =
    (pos', boxes')
  | otherwise = (pos, boxes)
  where
    dp = oneStep dir
    pos' = pos .+. dp
    load = boxesInTheWay2 pos boxes dir
    load' = Set.mapMonotonic (.+. dp) load
    boxes' = (boxes `Set.difference` load) `Set.union` load'

-- double-width boxes that are in the way of a move in the given direction
boxesInTheWay2 :: Position -> Set Position -> Direction -> Set Position
boxesInTheWay2 pos boxes dir = case oneStep dir of
    Position dx dy
      | dx < 0 -> boxesInTheWayLeft pos boxes
      | dx > 0 -> boxesInTheWayRight pos boxes
      | otherwise -> boxesInTheWayVertical pos boxes dy

-- double-width boxes that are in the way of a move to the left
boxesInTheWayLeft :: Position -> Set Position -> Set Position
boxesInTheWayLeft pos boxes =
    Set.fromList (takeWhile (flip Set.member boxes) (iterate (.+. dp) pos'))
  where
    pos' = pos .+. dp
    dp = Position (-2) 0

-- double-width boxes that are in the way of a move to the right
boxesInTheWayRight :: Position -> Set Position -> Set Position
boxesInTheWayRight pos boxes =
    Set.fromList (takeWhile (flip Set.member boxes) (iterate (.+. dp) pos'))
  where
    pos' = pos .+. Position 1 0
    dp = Position 2 0

-- double-width boxes that are in the way of a vertical move
boxesInTheWayVertical :: Position -> Set Position -> Int -> Set Position
boxesInTheWayVertical pos boxes dy =
    Set.unions $
        takeWhile (not . Set.null) $
        iterate (bvStep boxes dy) (bvStart boxes dy pos)

-- double-width box that p is pushing against (if any)
bvStart :: Set Position -> Int -> Position -> Set Position
bvStart boxes dy p =
    Set.fromList [p' |
        dx <- [-1, 0],
        let p' = p .+. Position dx dy,
        Set.member p' boxes]

-- double-width boxes that a double-width box in ps is pushing against
bvStep :: Set Position -> Int -> Set Position -> Set Position
bvStep boxes dy ps =
    Set.fromList [p' |
        p <- Set.elems ps,
        dx <- [-1, 0, 1],
        let p' = p .+. Position dx dy,
        Set.member p' boxes]

-- Debug output

showMoves2 :: Input -> String
showMoves2 input = case scaleUp input of
    (free_space, boxes, start, dirs) ->
        let
            state0 = (start, boxes)
            states = drop 1 $ scanl (move2 free_space) state0 dirs
        in
        unlines $
        (showWarehouse2 free_space state0:
        ["Move " ++ show dir ++ ":\n" ++ showWarehouse2 free_space state |
            (dir, state) <- zip dirs states])

showWarehouse2 :: Set Position -> State -> String
showWarehouse2 free_space (pos, boxes) = showBox box showPos
  where
    box0 = boundingBox free_space
    box = boundingBox [Position 0 0, maxCorner box0 .+. Position 2 1]
    showPos p
      | p == pos = '@'
      | Set.member p boxes = '['
      | Set.member (p .-. Position 1 0) boxes = ']'
      | Set.member p free_space = '.'
      | otherwise = '#'

testInputTiny :: String
testInputTiny = "\
    \#######\n\
    \#...#.#\n\
    \#.....#\n\
    \#..OO@#\n\
    \#..O..#\n\
    \#.....#\n\
    \#######\n\
    \\n\
    \<vv<<^^<<^^\n\
    \"

tests2 :: [(String, Int)]
tests2 = [(testInputTiny, 105+207+306), (testInputLarge, 9021)]

main :: IO ()
main = do
    s <- readFile "input/15.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
