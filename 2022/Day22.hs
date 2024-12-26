module Main where

import Utilities
import Geometry
import Parser
import Control.Applicative
import Data.Function
import Data.List
import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = (FieldMap, Path)
type Path = (Int, [(Turn, Int)])
data Turn = L | R
    deriving (Bounded, Enum, Read, Show)

data FieldMap = FieldMap {
    open :: Set Position,
    walls :: Set Position
    }
    deriving (Show)

parse :: String -> Input
parse s = case paragraphs s of
    [map_text, path_text] -> (field_map, runParser path path_line)
      where
        field_map = FieldMap {
            open = Set.fromList [p | (p, c) <- pcs, c == '.'],
            walls = Set.fromList [p | (p, c) <- pcs, c == '#']
            }
        pcs = readGrid map_text
        path_line = head (lines path_text)
    _ -> error "bad input"
  where
    path = (,) <$> nat <*> many ((,) <$> enumValue <*> nat)

-- Part One

data Direction = DRight | DDown | DLeft | DUp
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

turn :: Turn -> Direction -> Direction
turn R f = toEnum ((fromEnum f + 1) `mod` 4)
turn L f = toEnum ((fromEnum f - 1) `mod` 4)

oneStep :: Direction -> Position
oneStep DRight = Position 1 0
oneStep DDown = Position 0 1
oneStep DLeft = Position (-1) 0
oneStep DUp = Position 0 (-1)

-- Limits of rows and columns of the map

data RowsColumns = RowsColumns {
    left_x :: Map Int Int, -- for a given y
    right_x :: Map Int Int, -- for a given y
    top_y :: Map Int Int, -- for a given x
    bottom_y :: Map Int Int -- for a given x
    }
    deriving (Show)

mkRowsColumns :: FieldMap -> RowsColumns
mkRowsColumns m = RowsColumns {
    left_x = Map.map Set.findMin yxs,
    right_x = Map.map Set.findMax yxs,
    top_y = Map.map Set.findMin xys,
    bottom_y = Map.map Set.findMax xys
    }
  where
    xys = Map.fromListWith Set.union
        [(x, Set.singleton y) | Position x y <- ps]
    yxs = Map.fromListWith Set.union
        [(y, Set.singleton x) | Position x y <- ps]
    ps = Set.elems (allPositions m)

allPositions :: FieldMap -> Set Position
allPositions m = Set.union (open m) (walls m)

data State = State { pos :: Position, facing :: Direction }
    deriving (Show)

initState :: FieldMap -> State
initState m = State {
    pos = minimumBy (compare `on` row_col) (Set.elems (allPositions m)),
    facing = DRight
    }
  where
    row_col (Position x y) = (y, x)

nextPos :: RowsColumns -> Direction -> Position -> Position
nextPos rc DRight (Position x y)
  | x < right_x rc ! y = Position (x+1) y
  | otherwise = Position (left_x rc ! y) y
nextPos rc DDown (Position x y)
  | y < bottom_y rc ! x = Position x (y+1)
  | otherwise = Position x (top_y rc ! x)
nextPos rc DLeft (Position x y)
  | x > left_x rc ! y = Position (x-1) y
  | otherwise = Position (right_x rc ! y) y
nextPos rc DUp (Position x y)
  | y > top_y rc ! x = Position x (y-1)
  | otherwise = Position x (bottom_y rc ! x)

move :: FieldMap -> RowsColumns -> Int -> State -> State
move m rc n s = s { pos = last steps }
  where
    steps = takeWhile (\ p -> Set.member p (open m)) $
        take (n+1) $ iterate (nextPos rc (facing s)) $ pos s

segment :: FieldMap -> RowsColumns -> State -> (Turn, Int) -> State
segment m rc s (t, n) =
    move m rc n $ s { facing = turn t (facing s) }

doPath :: FieldMap -> State -> Path -> State
doPath m s (n, tns) = foldl (segment m rc) (move m rc n s) tns
  where
    rc = mkRowsColumns m

password :: State -> Int
password (State { pos = Position x y, facing = f }) = 
   1000*(y+1) + 4*(x+1) + fromEnum f

solve1 :: Input -> Int
solve1 (m, p) = password $ doPath m (initState m) p

testInput :: String
testInput = "\
    \        ...#\n\
    \        .#..\n\
    \        #...\n\
    \        ....\n\
    \...#.......#\n\
    \........#...\n\
    \..#....#....\n\
    \..........#.\n\
    \        ...#....\n\
    \        .....#..\n\
    \        .#......\n\
    \        ......#.\n\
    \\n\
    \10R5L5R10L4R5L5\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 6032)]

-- Part Two

-- Breaking the input into six square faces

type Face = Position -- divided by the side length of a square

sideLength :: FieldMap -> Int
sideLength m =
    round (sqrt (fromIntegral (Set.size (allPositions m) `div` 6) :: Double))

faces :: FieldMap -> Set Face
faces m = Set.map (face (sideLength m)) (allPositions m)

-- the face containing the position
face :: Int -> Position -> Face
face side (Position x y) = Position (x `div` side) (y `div` side)

-- A cube, representing the relationship between faces

data Cube = Cube {
    side_length :: Int,
    neighbours :: Map Face (Map Direction (Face, Turns))
    }
    deriving (Show)
type Turns = Int -- counterclockwise quarter turns

-- Fold up the map as a cube.
cube :: FieldMap -> Cube
cube m = Cube {
    side_length = sideLength m,
    neighbours = Map.fromList [(f, adjacent fs f) | f <- Set.elems fs]
    }
  where
    fs = faces m

-- How to move from a face to ith neighbours after folding.
adjacent :: Set Face -> Face -> Map Direction (Face, Turns)
adjacent fs f =
    Map.fromList [(dir, (f', n)) |
        (dirs, f') <- paths fs f,
        (dir, n) <- maybeToList (pathToJoin dirs)]

-- Paths from a start face to all faces that will be adjacent after folding.
paths :: Set Face -> Face -> [([Direction], Face)]
paths fs f0 = aux (Set.singleton f0) f0
  where
    aux visited f =
        [(dir:dirs, f'') |
            dir <- allValues, let f' = f .+. oneStep dir, Set.member f' fs,
            not (Set.member f' visited),
            (dirs, f'') <- ([], f'):aux (Set.insert f' visited) f']

-- Work out how the face reached with this list of directions connects
-- to the start face, if possible.  (The opposite face doesn't connect.)
pathToJoin :: [Direction] -> Maybe (Direction, Turns)
pathToJoin dirs
  | dot v v0 == 0 = Just (dir, n)
  | otherwise = Nothing
  where
    (v, up) = foldl moveTo (v0, up0) dirs
    dir = xyToDirection v
    n = 3 - fromEnum (xyToDirection up')
    up' = moveBack up dir
    v0 = Point3 0 0 1
    up0 = Point3 0 1 0

-- map a unit vector in the XY plane to a direction
xyToDirection :: Point3 -> Direction
xyToDirection (Point3 1 0 0) = DRight
xyToDirection (Point3 0 (-1) 0) = DDown
xyToDirection (Point3 (-1) 0 0) = DLeft
xyToDirection (Point3 0 1 0) = DUp
xyToDirection p = error $ "unexpected point " ++ show p

-- Manipulate unit 3-d vectors representing the current side and its
-- up direction.
moveTo :: (Point3, Point3) -> Direction -> (Point3, Point3)
moveTo (side, up) DRight = ((-1) *. side `cross` up, up)
moveTo (side, up) DLeft = (side `cross` up, up)
moveTo (side, up) DUp = (up, (-1) *. side)
moveTo (side, up) DDown = ((-1) *. up, side)

-- Move the up direction of the adjacent face back to the start so we
-- can see how it changed.
moveBack :: Point3 -> Direction -> Point3
moveBack (Point3 x y z) DRight = Point3 (-z) y x
moveBack (Point3 x y z) DLeft = Point3 z y (-x)
moveBack (Point3 x y z) DUp = Point3 x (-z) y
moveBack (Point3 x y z) DDown = Point3 x z (-y)

-- Moving around on the cubs

-- state we would like to move into after one step (ignoring walls)
nextState :: Cube -> State -> State
nextState c s = case dir of
    DRight
      | face_x < end -> s { pos = Position (x+1) y }
      | otherwise -> enterFace len new_face new_facing face_y
    DDown
      | face_y < end -> s { pos = Position x (y+1) }
      | otherwise -> enterFace len new_face new_facing opp_face_x
    DLeft
      | face_x > 0 -> s { pos = Position (x-1) y }
      | otherwise -> enterFace len new_face new_facing opp_face_y
    DUp
      | face_y > 0 -> s { pos = Position x (y-1) }
      | otherwise -> enterFace len new_face new_facing face_x
  where
    len = side_length c
    dir = facing s
    (new_face, n) = neighbours c ! face len p ! dir
    new_facing = turns n dir
    p@(Position x y) = pos s
    face_x = x `mod` len
    face_y = y `mod` len
    end = len - 1
    opp_face_x = end - face_x
    opp_face_y = end - face_y

turns :: Int -> Direction -> Direction
turns n dir = toEnum ((fromEnum dir + n) `mod` 4)

-- enter a face, moving in the given direction
-- n is offset from left edge relative to direction of movement
enterFace :: Int -> Face -> Direction -> Int -> State
enterFace len f dir n =
    State { pos = offset .+. len *. f, facing = dir }
  where
    offset = case dir of
        DRight -> Position 0 n
        DDown -> Position (end-n) 0
        DLeft -> Position end (end-n)
        DUp -> Position n end
    end = len-1

move2 :: FieldMap -> Cube -> Int -> State -> State
move2 m c n = last . takeWhile legal .  take (n+1) . iterate (nextState c)
  where
    legal s = Set.member (pos s) (open m)

segment2 :: FieldMap -> Cube -> State -> (Turn, Int) -> State
segment2 m c s (t, n) =
    move2 m c n $ s { facing = turn t (facing s) }

doPath2 :: FieldMap -> State -> Path -> State
doPath2 m s (n, tns) = foldl (segment2 m c) (move2 m c n s) tns
  where
    c = cube m

solve2 :: Input -> Int
solve2 (m, p) = password $ doPath2 m (initState m) p

tests2 :: [(String, Int)]
tests2 = [(testInput, 5031)]

main :: IO ()
main = do
    s <- readFile "input/22.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
