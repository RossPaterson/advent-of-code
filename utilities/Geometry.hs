-- | Simple linear algebra
module Geometry (
    -- * Classes
    NormedSpace(..), distance, composeVector,
    InnerProduct(..),
    Planar(..), unitVector,
    -- * Particular spaces
    -- ** Display coordinates
    Position(..), corners,
    readGrid, showGrid,
    -- ** Cartesian spaces of various dimensions
    Point2(..), polygonArea2,
    Point3(..), cross,
    Point4(..),
    -- ** Hexagonal tiling
    HexCoord(..),
    -- ** Diagonal square tiling
    Diagonal(..),
    positionToDiagonal,
    DiagonalPosition(..),
    diagonalPosition,
    -- * Axis-aligned boxes
    AABox,
    -- ** Construction
    singletonBox,
    boundingBox,
    -- ** Queries
    boxSize, boxElements, inBox,
    minCorner, maxCorner,
    showBox,
    -- ** Operations
    intersectBox,
    diffBox,
    growBox,
    shiftBox,
    )
    where

import Data.Foldable (toList)
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

infixr 7  *., .*.
infixl 6  .+., .-.

-- | A finite-dimensional module over the integers with a norm function
class (Eq v, Ord v) => NormedSpace v where
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

    -- | All the vectors with 'norm' of @1@.
    unitVectors :: [v]

    -- | A maximal selection of linearly independent unit vectors.
    basisVectors :: [v]

    -- | Decompose a vector into coefficients of the basis vectors.
    vectorComponents :: v -> [Int]

-- | A metric, defined as the 'norm' of the difference between the values.
distance :: NormedSpace v => v -> v -> Int
distance v1 v2 = norm (v1 .-. v2)

-- | Compose a vector from its components.
composeVector :: NormedSpace v => [Int] -> v
composeVector xs = foldr (.+.) zero (zipWith (*.) xs basisVectors)

-- | A module over the integers with an inner product
class (NormedSpace v) => InnerProduct v where
    -- | Inner product
    dot :: v -> v -> Int

-- | Two-dimensional plane divided into some number of equal-sized sectors.
-- The 'unitVectors' are the unit vectors at the start of each sector,
-- beginning with the unit vector to the right and proceeding in
-- counterclockwise order.
class (NormedSpace v) => Planar v where
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
unitVector :: (Planar v) => Int -> v
unitVector n = dirs!!(n `mod` length dirs)
  where
    dirs = unitVectors

instance NormedSpace Int where
    zero = 0
    (.+.) = (+)
    (.-.) = (-)
    (*.) = (*)
    norm = abs
    unitVectors = [1, -1]
    basisVectors = [1]
    vectorComponents n = [n]

-- | Point in 2-dimensional display space: x increases to the right,
-- y downwards
data Position = Position !Int !Int
    deriving (Eq, Ord, Show)

-- | `norm` is the Manhattan norm
instance NormedSpace Position where
    zero = Position 0 0
    Position x1 y1 .+. Position x2 y2 = Position (x1+x2) (y1+y2)
    Position x1 y1 .-. Position x2 y2 = Position (x1-x2) (y1-y2)
    r *. Position x y = Position (r*x) (r*y)

    norm (Position x y) = abs x + abs y

    unitVectors =
        [Position 1 0, Position 0 (-1), Position (-1) 0, Position 0 1]

    basisVectors = [Position 1 0, Position 0 1]

    vectorComponents (Position x y) = [x, y]

instance InnerProduct Position where
    dot (Position x1 y1) (Position x2 y2) = x1*x2 + y1*y2

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
    showBox (boundingBox (Map.keys m)) (flip (Map.findWithDefault def) m)

-- | Cartesian coordinate in 2-dimensional space
data Point2 = Point2 !Int !Int
    deriving (Eq, Ord, Show)

-- | `norm` is the Manhattan norm
instance NormedSpace Point2 where
    zero = Point2 0 0
    Point2 x1 y1 .+. Point2 x2 y2 = Point2 (x1+x2) (y1+y2)
    Point2 x1 y1 .-. Point2 x2 y2 = Point2 (x1-x2) (y1-y2)
    r *. Point2 x y = Point2 (r*x) (r*y)

    norm (Point2 x y) = abs x + abs y

    unitVectors =
        [Point2 1 0, Point2 0 1, Point2 (-1) 0, Point2 0 (-1)]

    basisVectors = [Point2 1 0, Point2 0 1]

    vectorComponents (Point2 x y) = [x, y]

instance InnerProduct Point2 where
    dot (Point2 x1 y1) (Point2 x2 y2) = x1*x2 + y1*y2

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

-- | Double the area of the polygon marked out with a cyclic path.
polygonArea2 :: [Point2] -> Int
polygonArea2 [] = 0
polygonArea2 (p:ps) = -- shoelace formula
    fromInteger $ abs $ sum $ zipWith determinant (p:ps) (ps ++ [p])
  where
    determinant (Point2 x1 y1) (Point2 x2 y2) =
        toInteger x1*toInteger y2 - toInteger x2*toInteger y1

-- | Cartesian coordinate in 3-dimensional space
data Point3 = Point3 !Int !Int !Int
    deriving (Eq, Ord, Show)

-- | `norm` is the Manhattan norm
instance NormedSpace Point3 where
    zero = Point3 0 0 0
    Point3 x1 y1 z1 .+. Point3 x2 y2 z2 = Point3 (x1+x2) (y1+y2) (z1+z2)
    Point3 x1 y1 z1 .-. Point3 x2 y2 z2 = Point3 (x1-x2) (y1-y2) (z1-z2)
    r *. Point3 x y z = Point3 (r*x) (r*y) (r*z)

    norm (Point3 x y z) = abs x + abs y + abs z

    unitVectors =
        [Point3 1 0 0, Point3 (-1) 0 0, Point3 0 1 0, Point3 0 (-1) 0,
         Point3 0 0 1, Point3 0 0 (-1)]

    basisVectors = [Point3 1 0 0, Point3 0 1 0, Point3 0 0 1]

    vectorComponents (Point3 x y z) = [x, y, z]

instance InnerProduct Point3 where
    dot (Point3 x1 y1 z1) (Point3 x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

-- | cross product of two vectors
cross :: Point3 -> Point3 -> Point3
cross (Point3 x1 y1 z1) (Point3 x2 y2 z2) =
    Point3 (y1*z2 - y2*z1) (z1*x2 - z2*x1) (x1*y2 - y1*x2)

-- | Cartesian coordinate in 4-dimensional space
data Point4 = Point4 !Int !Int !Int !Int
    deriving (Eq, Ord, Show)

-- | `norm` is the Manhattan norm
instance NormedSpace Point4 where
    zero = Point4 0 0 0 0
    Point4 x1 y1 z1 t1 .+. Point4 x2 y2 z2 t2 =
        Point4 (x1+x2) (y1+y2) (z1+z2) (t1+t2)
    Point4 x1 y1 z1 t1 .-. Point4 x2 y2 z2 t2 =
        Point4 (x1-x2) (y1-y2) (z1-z2) (t1-t2)
    r *. Point4 x y z t = Point4 (r*x) (r*y) (r*z) (r*t)

    norm (Point4 x y z t) = abs x + abs y + abs z + abs t

    unitVectors =
        [Point4 1 0 0 0, Point4 (-1) 0 0 0, Point4 0 1 0 0, Point4 0 (-1) 0 0,
         Point4 0 0 1 0, Point4 0 0 (-1) 0, Point4 0 0 0 1, Point4 0 0 0 (-1)]

    basisVectors =
        [Point4 1 0 0 0, Point4 0 1 0 0, Point4 0 0 1 0, Point4 0 0 0 1]

    vectorComponents (Point4 x y z t) = [x, y, z, t]

instance InnerProduct Point4 where
    dot (Point4 x1 y1 z1 t1) (Point4 x2 y2 z2 t2) =
        x1*x2 + y1*y2 + z1*z2 + t1*t2

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

-- | `norm` is distance in the trangular lattice
instance NormedSpace HexCoord where
    zero = HexCoord 0 0
    HexCoord x1 y1 .+. HexCoord x2 y2 = HexCoord (x1+x2) (y1+y2)
    HexCoord x1 y1 .-. HexCoord x2 y2 = HexCoord (x1-x2) (y1-y2)
    r *. HexCoord x y = HexCoord (r*x) (r*y)

    norm (HexCoord x y) = maximum [abs x, abs y, abs (x+y)]

    unitVectors =
        [HexCoord 1 0, HexCoord 0 1, HexCoord (-1) 1,
         HexCoord (-1) 0, HexCoord 0 (-1), HexCoord 1 (-1)]

    basisVectors = [HexCoord 1 0, HexCoord 0 1]

    vectorComponents (HexCoord x y) = [x, y]

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

-- | Point in a diagonal grid.
-- The first component is up and right; the second is down and right.
data Diagonal = Diagonal !Int !Int
    deriving (Eq, Ord, Show)

-- | `norm` is distance in the lattice
instance NormedSpace Diagonal where
    zero = Diagonal 0 0
    Diagonal a1 b1 .+. Diagonal a2 b2 = Diagonal (a1+a2) (b1+b2)
    Diagonal a1 b1 .-. Diagonal a2 b2 = Diagonal (a1-a2) (b1-b2)
    r *. Diagonal a b = Diagonal (r*a) (r*b)

    norm (Diagonal a b) = abs a + abs b

    unitVectors =
        [Diagonal 1 0, Diagonal 0 (-1), Diagonal (-1) 0, Diagonal 0 1]

    basisVectors = [Diagonal 1 0, Diagonal 0 1]

    vectorComponents (Diagonal x y) = [x, y]

instance InnerProduct Diagonal where
    dot (Diagonal a1 b1) (Diagonal a2 b2) = a1*a2 + b1*b2

-- | Embedding of 'Position' as the even points of 'Diagonal'.
-- This preserves the additive and scaling operations but not 'norm'.
positionToDiagonal :: Position -> Diagonal
positionToDiagonal (Position x y) = Diagonal (x-y) (x+y)

-- | Representation of a 'Position' near the point
data DiagonalPosition
    = At Position -- ^ at the position
    | DownRight Position -- ^ helf a step down and right from the position
    deriving (Eq, Ord, Show)

-- | A 'Position' near the point
diagonalPosition :: Diagonal -> DiagonalPosition
diagonalPosition (Diagonal u d)
  | even (d+u) = At (Position ((d+u) `div` 2) ((d-u) `div` 2))
  | otherwise = DownRight (Position ((d+u-1) `div` 2) ((d-u-1) `div` 2))

-- Component-wise lattice on the space

leq :: (NormedSpace v) => v -> v -> Bool
leq a b = and (zipWith (<=) (vectorComponents a) (vectorComponents b))

lub :: (NormedSpace v) => v -> v -> v
lub a b = composeVector (zipWith max (vectorComponents a) (vectorComponents b))

glb :: (NormedSpace v) => v -> v -> v
glb a b = composeVector (zipWith min (vectorComponents a) (vectorComponents b))

-- Axis-aligned boxes

-- | Non-empty axis-aligned box
data AABox a = AABox !a !a -- invariant: @lo `leq` hi@
    deriving (Eq, Ord, Show)

-- | smallest axis-aligned bounding box containing both boxes
instance (NormedSpace a) => Semigroup (AABox a) where
    AABox lo1 hi1 <> AABox lo2 hi2 = AABox (glb lo1 lo2) (lub hi1 hi2)

-- | An axis-aligned bounding box containing a single point
singletonBox :: v -> AABox v
singletonBox p = AABox p p

-- | Minimal axis-aligned box containing a non-empty collection of points
boundingBox :: (Foldable f, NormedSpace v) => f v -> AABox v
boundingBox ps
  | null xss = error "boundingBox: empty collection"
  | otherwise = AABox lo hi
  where
    xss = transpose (map vectorComponents (toList ps))
    lo = composeVector (map minimum xss)
    hi = composeVector (map maximum xss)

-- | The number of elements of the box
boxSize :: (NormedSpace v) => AABox v -> Int
boxSize (AABox lo hi) =
    product $ zipWith range_size (vectorComponents lo) (vectorComponents hi)
  where
    range_size l h = h - l + 1

-- | The elements of the box
boxElements :: (NormedSpace v) => AABox v -> [v]
boxElements (AABox lo hi) =
    map composeVector $ sequence $
        zipWith range (vectorComponents lo) (vectorComponents hi)
  where
    range l h = [l..h]

-- | Is the element inside the box?
inBox :: (NormedSpace v) => v -> AABox v -> Bool
inBox p (AABox lo hi) =
    and $ zipWith3 in_range
        (vectorComponents lo) (vectorComponents p) (vectorComponents hi)
  where
    in_range l v h = l <= v && v <= h

-- | Corner of the box with least values for all components
minCorner :: (NormedSpace v) => AABox v -> v
minCorner (AABox lo _) = lo

-- | Corner of the box with greatest values for all components
maxCorner :: (NormedSpace v) => AABox v -> v
maxCorner (AABox _ hi) = hi

-- | String representation of all the positions in the box
showBox :: AABox Position -> (Position -> Char) -> String
showBox (AABox (Position x_min y_min) (Position x_max y_max)) showPos =
    unlines [[showPos (Position x y) | x <- [x_min..x_max]] |
        y <- [y_min..y_max]]

-- | The intersection of two axis-aligned bounding boxes, if they overlap.
intersectBox :: (NormedSpace v) => AABox v -> AABox v -> Maybe (AABox v)
intersectBox (AABox lo1 hi1) (AABox lo2 hi2)
  | lo `leq` hi = Just (AABox lo hi)
  | otherwise = Nothing
  where
    lo = lub lo1 lo2
    hi = glb hi1 hi2

-- | Subtraction of boxes, producing zero or more boxes.
-- For an /n/-dimensional space, this may produce up to 2/n/ boxes.
diffBox :: (NormedSpace v) => AABox v -> AABox v -> [AABox v]
diffBox (AABox lo1 hi1) (AABox lo2 hi2) =
    [AABox (composeVector los) (composeVector his) |
        (los, his) <- diffRanges
            (vectorComponents lo1) (vectorComponents hi1)
            (vectorComponents lo2) (vectorComponents hi2)]

diffRanges :: [Int] -> [Int] -> [Int] -> [Int] -> [([Int], [Int])]
diffRanges (lo1:lo1s) (hi1:hi1s) (lo2:lo2s) (hi2:hi2s) =
    [(lo1:lo1s, bottom_hi:hi1s) | lo1 <= bottom_hi] ++
    [(common_lo:los, common_hi:his) |
        common_lo <= common_hi,
        (los, his) <- diffRanges lo1s hi1s lo2s hi2s] ++
    [(top_lo:lo1s, hi1:hi1s) | top_lo <= hi1]
  where
    bottom_hi = min hi1 (lo2-1)
    common_lo = max lo1 lo2
    common_hi = min hi1 hi2
    top_lo = max lo1 (hi2+1)
diffRanges _ _ _ _ = []

-- | Make the box @n@ positions bigger in each direction.
growBox :: (NormedSpace v) => Int -> AABox v -> AABox v
growBox n (AABox lo hi) = AABox (lo .-. delta) (hi .+. delta)
  where
    delta = n*. foldr1 (.+.) basisVectors

-- | Move the box by delta.
shiftBox :: (NormedSpace v) => v -> AABox v -> AABox v
shiftBox delta (AABox lo hi) = AABox (lo .+. delta) (hi .+. delta)
