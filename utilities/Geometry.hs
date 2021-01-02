-- | Simple linear algebra
module Geometry (
    -- * Classes
    Module(..),
    NormedModule(..), distance,
    Planar(..), unitVector,
    -- * Particular spaces
    -- ** Display coordinates
    Position(..), corners,
    readGrid, showGrid,
    -- ** Cartesian spaces of various dimensions
    Point2(..),
    Point3(..),
    Point4(..),
    -- ** Hexagonal tiling
    HexCoord(..),
    )
    where

import Data.Map (Map)
import qualified Data.Map as Map

infixr 7  *., .*.
infixl 6  .+., .-.

-- | Module over the integers
class Module v where
    -- | The origin
    zero :: v
    -- | Addition, an associative operation with identity 'zero'
    (.+.) :: v -> v -> v
    -- | Subtraction, the inverse of addition
    (.-.) :: v -> v -> v
    -- | Scalar multiplication, distributing over 'zero' and addition
    (*.) :: Int -> v -> v

    v1 .-. v2 = v1 .+. (-1) *. v2

-- | Module over the integers with a norm function
class Module v => NormedModule v where
    -- | A nonnegative-valued function satisfying the triangle inequality
    norm :: v -> Int

    -- | All the vectors with 'norm' of @1@.
    unitVectors :: [v]

-- | A metric, defined as the 'norm' of the difference between the values.
distance :: NormedModule v => v -> v -> Int
distance v1 v2 = norm (v1 .-. v2)

-- | Two-dimensional plane divided into some number of equal-sized sectors.
-- The 'unitVectors' are the unit vectors at the start of each sector,
-- beginning with the unit vector to the right and proceeding in
-- counterclockwise order.
class NormedModule v => Planar v where
    -- | The sector of a point, counting from 0.
    sector :: v -> Int

    -- | Rotate the point counterclockwise by the given number of sectors.
    rotateSectors :: Int -> v -> v
    rotateSectors n p = p .*. unitVector n

    -- | Rotate the point counterclockwise by the given number of degrees.
    -- A given space will support only multiples of the sector size.
    rotateDegrees :: Int -> v -> v
    rotateDegrees deg p
      | deg*nsectors `mod` 360 == 0 =
        p .*. unitVector (deg*nsectors `div` 360)
      | otherwise = error $
        "The type does not support rotation by " ++ show deg ++ " degrees"
      where
        nsectors = length (unitVectors `asTypeOf` [p])

    -- | Multiplication in the complex plane.
    (.*.) :: v -> v -> v

-- | Unit vector at the start of sector @n@.
unitVector :: Planar v => Int -> v
unitVector n = dirs!!(n `mod` length dirs)
  where
    dirs = unitVectors

-- | Point in 2-dimensional display space: x increases to the right,
-- y downwards
data Position = Position !Int !Int
    deriving (Eq, Ord, Show)

instance Module Position where
    zero = Position 0 0
    Position x1 y1 .+. Position x2 y2 = Position (x1+x2) (y1+y2)
    Position x1 y1 .-. Position x2 y2 = Position (x1-x2) (y1-y2)
    r *. Position x y = Position (r*x) (r*y)

-- | `norm` is the Manhattan norm
instance NormedModule Position where
    norm (Position x y) = abs x + abs y

    unitVectors =
        [Position 1 0, Position 0 (-1), Position (-1) 0, Position 0 1]

-- | four sectors of 90 degrees each
instance Planar Position where
    rotateSectors n p = p .*. (unitVectors!!(n `mod` 4))

    rotateDegrees deg
      | deg `mod` 90 == 0 = rotateSectors (deg `div` 90)
      | otherwise = error $
        "Position does not support rotation by " ++ show deg ++ " degrees"

    sector (Position x y)
      | x <= 0 && y < 0 = 1
      | x < 0 && y >= 0 = 2
      | x >= 0 && y > 0 = 3
      | otherwise = 0

    Position a b .*. Position c d = Position (a*c - b*d) (a*d + b*c)

-- | Minimal vectors for the corner directions
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

-- | Cartesian coordinate in 2-dimensional space
data Point2 = Point2 !Int !Int
    deriving (Eq, Ord, Show)

instance Module Point2 where
    zero = Point2 0 0
    Point2 x1 y1 .+. Point2 x2 y2 = Point2 (x1+x2) (y1+y2)
    Point2 x1 y1 .-. Point2 x2 y2 = Point2 (x1-x2) (y1-y2)
    r *. Point2 x y = Point2 (r*x) (r*y)

-- | `norm` is the Manhattan norm
instance NormedModule Point2 where
    norm (Point2 x y) = abs x + abs y

    unitVectors =
        [Point2 1 0, Point2 0 1, Point2 (-1) 0, Point2 0 (-1)]

-- | four sectors of 90 degrees each
instance Planar Point2 where
    rotateSectors n p = p .*. (unitVectors!!(n `mod` 4))

    rotateDegrees deg
      | deg `mod` 90 == 0 = rotateSectors (deg `div` 90)
      | otherwise = error $
        "Point2 does not support rotation by " ++ show deg ++ " degrees"

    sector (Point2 x y)
      | x <= 0 && y > 0 = 1
      | x < 0 && y <= 0 = 2
      | x >= 0 && y < 0 = 3
      | otherwise = 0

    Point2 a b .*. Point2 c d = Point2 (a*c - b*d) (a*d + b*c)

-- | Cartesian coordinate in 3-dimensional space
data Point3 = Point3 !Int !Int !Int
    deriving (Eq, Ord, Show)

instance Module Point3 where
    zero = Point3 0 0 0
    Point3 x1 y1 z1 .+. Point3 x2 y2 z2 = Point3 (x1+x2) (y1+y2) (z1+z2)
    Point3 x1 y1 z1 .-. Point3 x2 y2 z2 = Point3 (x1-x2) (y1-y2) (z1-z2)
    r *. Point3 x y z = Point3 (r*x) (r*y) (r*z)

-- | `norm` is the Manhattan norm
instance NormedModule Point3 where
    norm (Point3 x y z) = abs x + abs y + abs z

    unitVectors =
        [Point3 1 0 0, Point3 (-1) 0 0, Point3 0 1 0, Point3 0 (-1) 0,
         Point3 0 0 1, Point3 0 0 (-1)]

-- | Cartesian coordinate in 4-dimensional space
data Point4 = Point4 !Int !Int !Int !Int
    deriving (Eq, Ord, Show)

instance Module Point4 where
    zero = Point4 0 0 0 0
    Point4 x1 y1 z1 t1 .+. Point4 x2 y2 z2 t2 =
        Point4 (x1+x2) (y1+y2) (z1+z2) (t1+t2)
    Point4 x1 y1 z1 t1 .-. Point4 x2 y2 z2 t2 =
        Point4 (x1-x2) (y1-y2) (z1-z2) (t1-t2)
    r *. Point4 x y z t = Point4 (r*x) (r*y) (r*z) (r*t)

-- | `norm` is the Manhattan norm
instance NormedModule Point4 where
    norm (Point4 x y z t) = abs x + abs y + abs z + abs t

    unitVectors =
        [Point4 1 0 0 0, Point4 (-1) 0 0 0, Point4 0 1 0 0, Point4 0 (-1) 0 0,
         Point4 0 0 1 0, Point4 0 0 (-1) 0, Point4 0 0 0 1, Point4 0 0 0 (-1)]

-- | Axial (or skewed) coordinate in a 2-dimensional hexagonal grid
-- (<https://www.redblobgames.com/grids/hexagons/>),
-- or equivalently a trangular lattice
--
-- * @'HexCoord' 1 0@ is the unit vector to the right.
--
-- * @'HexCoord' 0 1@ is that vector rotated 60 degrees counterclockwise.
--
data HexCoord = HexCoord !Int !Int
    deriving (Eq, Ord, Show)

instance Module HexCoord where
    zero = HexCoord 0 0
    HexCoord x1 y1 .+. HexCoord x2 y2 = HexCoord (x1+x2) (y1+y2)
    HexCoord x1 y1 .-. HexCoord x2 y2 = HexCoord (x1-x2) (y1-y2)
    r *. HexCoord x y = HexCoord (r*x) (r*y)

instance NormedModule HexCoord where
    norm (HexCoord x y) = maximum [abs x, abs y, abs (x+y)]

    unitVectors =
        [HexCoord 1 0, HexCoord 0 1, HexCoord (-1) 1,
         HexCoord (-1) 0, HexCoord 0 (-1), HexCoord 1 (-1)]

-- | six sectors of 60 degrees each
instance Planar HexCoord where
    rotateSectors n p = p .*. (unitVectors!!(n `mod` 6))

    rotateDegrees deg
      | deg `mod` 60 == 0 = rotateSectors (deg `div` 60)
      | otherwise = error $
        "HexCoord does not support rotation by " ++ show deg ++ " degrees"

    sector (HexCoord a b)
      | a <= 0 && a+b > 0 = 1
      | a+b <= 0 && b > 0 = 2
      | b <= 0 && a < 0 = 3
      | a >= 0 && a+b < 0 = 4
      | a+b >= 0 && b < 0 = 5
      | otherwise = 0

    HexCoord a b .*. HexCoord c d = HexCoord (a*c - b*d) (a*d + b*c + b*d)
