-- | Simple linear algebra
module Geometry (
    -- * Normed modules over integers
    NormedModule(..), distance,
    -- * Display coordinates
    Position(..), cardinalDirections, corners,
    readGrid, showGrid,
    -- * Cartesian spaces of various dimensions
    Point2(..),
    Point3(..),
    Point4(..),
    -- * Hexagonal tiling
    HexCoord, hexDirection,
    )
    where

import Data.Map (Map)
import qualified Data.Map as Map

infixr 7  *.
infixl 6  .+., .-.

-- | Module over the integers with a norm function
class NormedModule v where
    -- | The origin
    zero :: v
    -- | Addition, an associative operation with identity 'zero'
    (.+.) :: v -> v -> v
    -- | Subtraction, the inverse of addition
    (.-.) :: v -> v -> v
    -- | Scalar multiplication, distributing over 'zero' and addition
    (*.) :: Int -> v -> v
    -- | A nonnegative-valued function satisfying the triangle inequality
    norm :: v -> Int

    v1 .-. v2 = v1 .+. (-1) *. v2

-- | A metric, defined as the 'norm' of the difference between the values.
distance :: NormedModule v => v -> v -> Int
distance v1 v2 = norm (v1 .-. v2)

-- | Point in 2-dimensional display space: x increases to the right,
-- y downwards
data Position = Position !Int !Int
    deriving (Eq, Ord, Show)

-- | `norm` is the Manhattan norm
instance NormedModule Position where
    zero = Position 0 0
    Position x1 y1 .+. Position x2 y2 = Position (x1+x2) (y1+y2)
    Position x1 y1 .-. Position x2 y2 = Position (x1-x2) (y1-y2)
    r *. Position x y = Position (r*x) (r*y)
    norm (Position x y) = abs x + abs y

-- | Unit vectors for the cardinal directions
cardinalDirections :: [Position]
cardinalDirections =
    [Position 0 (-1), Position 1 0, Position 0 1, Position (-1) 0]

-- | Unit vectors for the corner directions
corners :: [Position]
corners = [Position 1 (-1), Position 1 1, Position (-1) 1, Position (-1) (-1)]

-- | Read a 2-dimensional grid, with (0, 0) in the top left corner
readGrid :: String -> [(Position, Char)]
readGrid s =
    [(Position x y, c) | (y, l) <- zip [0..] (lines s), (x, c) <- zip [0..] l]

-- | String representation of smallest grid containing the points in the map
showGrid :: Char -> Map Position Char -> String
showGrid def m
  | Map.null m = ""
  | otherwise =
    unlines [[Map.findWithDefault def (Position x y) m |
        x <- [minX..maxX]] | y <- [minY..maxY]]
  where
    minX = minimum [x | Position x _ <- Map.keys m]
    maxX = maximum [x | Position x _ <- Map.keys m]
    minY = minimum [y | Position _ y <- Map.keys m]
    maxY = maximum [y | Position _ y <- Map.keys m]

-- | Point in 2-dimensional space
data Point2 = Point2 !Int !Int
    deriving (Eq, Ord, Show)

-- | `norm` is the Manhattan norm
instance NormedModule Point2 where
    zero = Point2 0 0
    Point2 x1 y1 .+. Point2 x2 y2 = Point2 (x1+x2) (y1+y2)
    Point2 x1 y1 .-. Point2 x2 y2 = Point2 (x1-x2) (y1-y2)
    r *. Point2 x y = Point2 (r*x) (r*y)
    norm (Point2 x y) = abs x + abs y

-- | Point in 3-dimensional space
data Point3 = Point3 !Int !Int !Int
    deriving (Eq, Ord, Show)

-- | `norm` is the Manhattan norm
instance NormedModule Point3 where
    zero = Point3 0 0 0
    Point3 x1 y1 z1 .+. Point3 x2 y2 z2 = Point3 (x1+x2) (y1+y2) (z1+z2)
    Point3 x1 y1 z1 .-. Point3 x2 y2 z2 = Point3 (x1-x2) (y1-y2) (z1-z2)
    r *. Point3 x y z = Point3 (r*x) (r*y) (r*z)
    norm (Point3 x y z) = abs x + abs y + abs z

-- | Point in 4-dimensional space
data Point4 = Point4 !Int !Int !Int !Int
    deriving (Eq, Ord, Show)

-- | `norm` is the Manhattan norm
instance NormedModule Point4 where
    zero = Point4 0 0 0 0
    Point4 x1 y1 z1 t1 .+. Point4 x2 y2 z2 t2 =
        Point4 (x1+x2) (y1+y2) (z1+z2) (t1+t2)
    Point4 x1 y1 z1 t1 .-. Point4 x2 y2 z2 t2 =
        Point4 (x1-x2) (y1-y2) (z1-z2) (t1-t2)
    r *. Point4 x y z t = Point4 (r*x) (r*y) (r*z) (r*t)
    norm (Point4 x y z t) = abs x + abs y + abs z + abs t

-- | Skew coordinate in a 2-dimensional hexagonal tiling
-- (= axial coordinate at https://www.redblobgames.com/grids/hexagons/)
data HexCoord = HexCoord !Int !Int
    deriving (Eq, Ord, Show)

instance NormedModule HexCoord where
    zero = HexCoord 0 0
    HexCoord x1 y1 .+. HexCoord x2 y2 = HexCoord (x1+x2) (y1+y2)
    HexCoord x1 y1 .-. HexCoord x2 y2 = HexCoord (x1-x2) (y1-y2)
    r *. HexCoord x y = HexCoord (r*x) (r*y)
    norm (HexCoord x y) = maximum [abs x, abs y, abs (x+y)]

-- | Single step in a direction in hexagonal space
hexDirection :: Int -> HexCoord
hexDirection n = hexDirections!!(n `mod` 6)

-- one step in the skew coordinate system, in a numbered direction:
--
--   2 | 1 |
--   3 | * | 0
--     | 4 | 5
--
hexDirections :: [HexCoord]
hexDirections = [
    HexCoord 1 0, HexCoord 0 1, HexCoord (-1) 1,
    HexCoord (-1) 0, HexCoord 0 (-1), HexCoord 1 (-1)]
