-- | Utilities for 2- and 3-dimensional geometry
module Geometry (
    -- * Two dimensions
    Point2(..),
    zero2, plus2, minus2, negate2, times2, manhattan2, dist2,
    readGrid, showGrid,
    -- * Three dimensions
    Point3(..),
    zero3, plus3, minus3, negate3, times3, manhattan3, dist3,
    )
    where

import Data.Map (Map)
import qualified Data.Map as Map

-- | Point in 2-D space
data Point2 = Point2 !Int !Int
    deriving (Eq, Ord, Show)

-- | The origin
zero2 :: Point2
zero2 = Point2 0 0

-- | Component-wise addition
plus2 :: Point2 -> Point2 -> Point2
plus2 (Point2 x1 y1) (Point2 x2 y2) = Point2 (x1+x2) (y1+y2)

-- | Component-wise subtraction
minus2 :: Point2 -> Point2 -> Point2
minus2 (Point2 x1 y1) (Point2 x2 y2) = Point2 (x1-x2) (y1-y2)

-- | Component-wise negation
negate2 :: Point2 -> Point2
negate2 (Point2 x y) = Point2 (- x) (- y)

-- | Scalar multiplication
times2 :: Int -> Point2 -> Point2
times2 r (Point2 x y) = Point2 (r*x) (r*y)

-- | Manhattan metric
manhattan2 :: Point2 -> Int
manhattan2 (Point2 x y) = abs x + abs y

-- | Distance by Manhattan metric
dist2 :: Point2 -> Point2 -> Int
dist2 (Point2 x1 y1) (Point2 x2 y2) = abs (x1-x2) + abs (y1-y2)

-- | Read a 2-dimensional grid, with (0, 0) in the top left corner
readGrid :: String -> [(Point2, Char)]
readGrid s =
    [(Point2 x y, c) | (y, l) <- zip [0..] (lines s), (x, c) <- zip [0..] l]

-- | String representation of smallest grid containing the points in the map
showGrid :: Char -> Map Point2 Char -> String
showGrid def m =
    unlines [[showPoint (Point2 x y) | x <- [minX..maxX]] | y <- [minY..maxY]]
  where
    showPoint p = Map.findWithDefault def p m
    minX = minimum [x | Point2 x _ <- Map.keys m]
    maxX = maximum [x | Point2 x _ <- Map.keys m]
    minY = minimum [y | Point2 _ y <- Map.keys m]
    maxY = maximum [y | Point2 _ y <- Map.keys m]

-- | Point in 3-D space
data Point3 = Point3 !Int !Int !Int
    deriving (Eq, Ord, Show)

-- | The origin
zero3 :: Point3
zero3 = Point3 0 0 0

-- | Component-wise addition
plus3 :: Point3 -> Point3 -> Point3
plus3 (Point3 x1 y1 z1) (Point3 x2 y2 z2) = Point3 (x1+x2) (y1+y2) (z1+z2)

-- | Component-wise subtraction
minus3 :: Point3 -> Point3 -> Point3
minus3 (Point3 x1 y1 z1) (Point3 x2 y2 z2) = Point3 (x1-x2) (y1-y2) (z1-z2)

-- | Component-wise negation
negate3 :: Point3 -> Point3
negate3 (Point3 x y z) = Point3 (- x) (- y) (- z)

-- | Scalar multiplication
times3 :: Int -> Point3 -> Point3
times3 r (Point3 x y z) = Point3 (r*x) (r*y) (r*z)

-- | Manhattan metric
manhattan3 :: Point3 -> Int
manhattan3 (Point3 x y z) = abs x + abs y + abs z

-- | Distance by Manhattan metric
dist3 :: Point3 -> Point3 -> Int
dist3 (Point3 x1 y1 z1) (Point3 x2 y2 z2) =
    abs (x1-x2) + abs (y1-y2) + abs (z1-z2)
