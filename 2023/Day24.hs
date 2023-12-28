module Main where

import Geometry
import Parser
import Utilities
import Control.Applicative
import Data.List
import Data.Maybe

-- Input processing

data Particle v = Particle { position :: v, velocity :: v }
    deriving (Show)

instance Functor Particle where
    fmap f (Particle p v) = Particle (f p) (f v)

type Input = [Particle Point3]

parse :: String -> Input
parse = map (runParser particle) . lines
  where
    particle = Particle <$> point <* string " @" <* some space <*> point
    point = Point3 <$> int <* comma <*> int <* comma <*> int
    comma = char ',' <* some space

-- Part One

-- project onto the x-y plane, ignoring the z-coordinate
project :: Point3 -> Point2
project (Point3 x y _) = Point2 x y

-- line with equation y = a*x + b
data Line2 = Line2 Rational Rational
    deriving (Show)

line2 :: Particle Point2 -> Line2
line2 (Particle (Point2 px py) (Point2 vx vy)) = Line2 a b
  where
    a = toRational vy/toRational vx
    b = toRational py - a*toRational px

intersect2 :: Particle Point2 -> Particle Point2 -> Maybe (Rational, Rational)
intersect2 p1 p2
  | a1 == a2 = Nothing
  | t1 < 0 || t2 < 0 = Nothing
  | otherwise = Just pt
  where
    det = a1*b2 - a2*b1
    pt = ((b2 - b1)/(a1 - a2), det/(a1 - a2))
    Line2 a1 b1 = line2 p1
    Line2 a2 b2 = line2 p2
    t1 = timeAt p1 pt
    t2 = timeAt p2 pt

-- time when the particle passed through the point
timeAt :: Particle Point2 -> (Rational, Rational) -> Rational
timeAt (Particle (Point2 px _) (Point2 vx _)) (x, _) =
    (x - toRational px)/toRational vx

intersections :: [Particle Point2] -> [(Rational, Rational)]
intersections particles =
    [p |
        p1:rest <- tails particles, p2 <- rest,
        p <- maybeToList (intersect2 p1 p2)]

-- Is the point within the test area?
within :: Int -> Int -> (Rational, Rational) -> Bool
within lo hi (x, y) =
    toRational lo <= x && x <= toRational hi &&
    toRational lo <= y && y <= toRational hi

intersectionsWithin :: Int -> Int -> [Particle Point3] -> Int
intersectionsWithin lo hi =
    length . filter (within lo hi) . intersections . map (fmap project)

solve1 :: Input -> Int
solve1 = intersectionsWithin 200000000000000 400000000000000

testInput :: String
testInput = "\
    \19, 13, 30 @ -2,  1, -2\n\
    \18, 19, 22 @ -1, -1, -2\n\
    \20, 25, 34 @ -2, -2, -4\n\
    \12, 31, 28 @ -1, -2, -1\n\
    \20, 19, 15 @  1, -5, -3\n"

tests1 :: [((Int, Int, String), Int)]
tests1 = [((7, 27, testInput), 2)]

-- Part Two

{-
We are told that there is a rock particle (rp, rv) such that for each
other particle (p, v), there is an integral time t such that

rp + t*rv = p + t*v

We don't need many of the particles: for n particles, we have 3n
equations in 6 + n variables (the rock's start position and velocity,
and a collision time for each of the particles), so n=3 is enough to
fully constrain the solution.

Rearranging the collision equation,

rp - p = t*(v - rv)

and thus we can eliminate t using the cross product:

(rp - p) x (v - rv) = 0
<=>
rp x v - rp x rv - p x v + p x rv = 0
<=>
rp x v + p x rv = rp x rv + p x v

The quadratic term is the same in all these equations, so we can eliminate
it by subtracting one of these equations from the others.

-}

-- linear and constant terms
data Row a = Row { linear :: [a], constant :: a }
    deriving (Show)

instance Functor Row where
    fmap f (Row as c) = Row (map f as) (f c)

-- subtraction of rows
minusRow :: (Num a) => Row a -> Row a -> Row a
minusRow (Row lhs1 rhs1) (Row lhs2 rhs2) =
    Row (zipWith (-) lhs1 lhs2) (rhs1 - rhs2)

-- The linear part of the collision equation for a single particle.
-- The quadratic term is omitted, because it will be eliminated.
-- variables: [rpx, rpy, rpz, rvx, rvy, rvz]
collisionEquations :: Particle Point3 -> [Row Rational]
collisionEquations (Particle p v) =
    -- rp x v + p x rv = rp x rv + p x v
    map (fmap toRational) (zipWith Row linear_parts constant_parts)
  where
    linear_parts =
        [vectorComponents (cross v unit) ++ vectorComponents (cross unit p) |
            unit <- basisVectors]
    constant_parts = vectorComponents (cross p v)

-- Generate 6 equations in the position and velocity of the rock,
-- by eliminating the quadratic term from the equations for the first
-- 3 particles.
allCollisionEquations :: [Particle Point3] -> [Row Rational]
allCollisionEquations ps = case map collisionEquations (take 3 ps) of
    [] -> error "no equations"
    eqns1:eqnss ->
        [eqn' | eqns <- eqnss, eqn' <- zipWith minusRow eqns eqns1]

-- Solve a system of linear equations by Gaussian elimination.
-- (assuming coefficients and solutions are integers)
solveLinear :: (Eq a, Fractional a) => [Row a] -> [a]
solveLinear [] = []
solveLinear eqns = -- choose a row without a leading zero as the pivot
    case partition ((== 0) . head . linear) eqns of
    (zeroes, pivot@(Row (cp:lhsp) rhsp):rest) -> value:values
      where
        values =
            solveLinear ([Row lhs rhs | Row (_:lhs) rhs <- zeroes] ++
                map (eliminate pivot) rest)
        value = (rhsp - sum (zipWith (*) lhsp values)) / cp
    _ -> error "can't find a pivot"

-- Eliminate the first variable from an equation by subtracting as
-- scaling of the pivot equation.
eliminate :: (Eq a, Fractional a) => Row a -> Row a -> Row a
eliminate (Row (cp:lhsp) rhsp) (Row (c:lhs) rhs) =
    minusRow (Row lhs rhs) (fmap (*(c/cp)) (Row lhsp rhsp))
eliminate _ _ = error "elimination with empty equations"

-- a particle that collides with the first three particles
rock :: [Particle Point3] -> Particle Point3
rock ps =
    Particle (composeVector (take 3 soln))
        (composeVector (take 3 (drop 3 soln)))
  where
    soln = map floor $ solveLinear $ allCollisionEquations ps

summary :: Particle Point3 -> Int
summary (Particle (Point3 x y z) _) = x + y + z

solve2 :: Input -> Int
solve2 = summary . rock

tests2 :: [(String, Int)]
tests2 = [(testInput, 47)]

testSolution :: Particle Point3
testSolution = Particle (Point3 24 13 10) (Point3 (-3) 1 2)

main :: IO ()
main = do
    s <- readFile "input/24.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (\(lo,hi,t) -> intersectionsWithin lo hi (parse t)) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
