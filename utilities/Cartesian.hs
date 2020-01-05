-- | Cartesian coordinates
module Cartesian (
    -- * Normed vector spaces
    NormedVectorSpace(..), distance,
    -- * Display coordinates
    Position(..),
    readGrid, showGrid,
    -- * Various dimensions
    Point2(..),
    Point3(..),
    Point4(..),
    )
    where

import Data.Map (Map)
import qualified Data.Map as Map

infixr 7  *.
infixl 6  .+., .-.

-- | Normed vector space
class NormedVectorSpace v where
    -- | The origin
    zero :: v
    -- | Component-wise addition
    (.+.) :: v -> v -> v
    -- | Component-wise subtraction
    (.-.) :: v -> v -> v
    -- | Scalar multiplication
    (*.) :: Int -> v -> v
    -- | Manhattan metric
    norm :: v -> Int

    v1 .-. v2 = v1 .+. (-1) *. v2

-- | Distance by Manhattan metric
distance :: NormedVectorSpace v => v -> v -> Int
distance v1 v2 = norm (v1 .-. v2)

-- | Point in 2-D display space: x increases to right, y downwards
data Position = Position !Int !Int
    deriving (Eq, Ord, Show)

instance NormedVectorSpace Position where
    zero = Position 0 0
    Position x1 y1 .+. Position x2 y2 = Position (x1+x2) (y1+y2)
    Position x1 y1 .-. Position x2 y2 = Position (x1-x2) (y1-y2)
    r *. Position x y = Position (r*x) (r*y)
    norm (Position x y) = abs x + abs y

-- | Read a 2-dimensional grid, with (0, 0) in the top left corner
readGrid :: String -> [(Position, Char)]
readGrid s =
    [(Position x y, c) | (y, l) <- zip [0..] (lines s), (x, c) <- zip [0..] l]

-- | String representation of smallest grid containing the points in the map
showGrid :: Char -> Map Position Char -> String
showGrid def m =
    unlines [[Map.findWithDefault def (Position x y) m |
        x <- [minX..maxX]] | y <- [minY..maxY]]
  where
    minX = minimum [x | Position x _ <- Map.keys m]
    maxX = maximum [x | Position x _ <- Map.keys m]
    minY = minimum [y | Position _ y <- Map.keys m]
    maxY = maximum [y | Position _ y <- Map.keys m]

-- | Point in 2-D space
data Point2 = Point2 !Int !Int
    deriving (Eq, Ord, Show)

instance NormedVectorSpace Point2 where
    zero = Point2 0 0
    Point2 x1 y1 .+. Point2 x2 y2 = Point2 (x1+x2) (y1+y2)
    Point2 x1 y1 .-. Point2 x2 y2 = Point2 (x1-x2) (y1-y2)
    r *. Point2 x y = Point2 (r*x) (r*y)
    norm (Point2 x y) = abs x + abs y

-- | Point in 3-D space
data Point3 = Point3 !Int !Int !Int
    deriving (Eq, Ord, Show)

instance NormedVectorSpace Point3 where
    zero = Point3 0 0 0
    Point3 x1 y1 z1 .+. Point3 x2 y2 z2 = Point3 (x1+x2) (y1+y2) (z1+z2)
    Point3 x1 y1 z1 .-. Point3 x2 y2 z2 = Point3 (x1-x2) (y1-y2) (z1-z2)
    r *. Point3 x y z = Point3 (r*x) (r*y) (r*z)
    norm (Point3 x y z) = abs x + abs y + abs z

-- | Point in 4-D space
data Point4 = Point4 !Int !Int !Int !Int
    deriving (Eq, Ord, Show)

instance NormedVectorSpace Point4 where
    zero = Point4 0 0 0 0
    Point4 x1 y1 z1 t1 .+. Point4 x2 y2 z2 t2 =
        Point4 (x1+x2) (y1+y2) (z1+z2) (t1+t2)
    Point4 x1 y1 z1 t1 .-. Point4 x2 y2 z2 t2 =
        Point4 (x1-x2) (y1-y2) (z1-z2) (t1-t2)
    r *. Point4 x y z t = Point4 (r*x) (r*y) (r*z) (r*t)
    norm (Point4 x y z t) = abs x + abs y + abs z + abs t
