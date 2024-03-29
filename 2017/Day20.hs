module Main where

import Parser
import Utilities
import Data.List
import Data.Ord
import Data.Set (Set)
import qualified Data.Set as Set

data Vector = Vector { xc :: !Int, yc :: !Int, zc :: !Int }
    deriving (Eq, Ord, Show)

-- Because we're asked about asymptotic behaviour, we need to solve this
-- particle system analytically.  Each coordinate evolves independently.
-- Its values at step n will be
--
--     a_n = a_0
--     v_n = a_0*n + v_0
--     p_n = a_0*(n*(n+1) `div` 2) + v_0*n + p_0
--
-- Asymptotically, position will be dominated by initial acceleration,
-- then initial velocity, then initial position, so we put them in that
-- order for the Ord instance.
data Motion a = Motion { acceleration :: a, velocity :: a, position :: a }
    deriving (Eq, Ord, Show)

instance Functor Motion where
    fmap f (Motion a v p) = Motion (f a) (f v) (f p)

type Particle = Motion Vector
type System = [Particle]

type Input = System

parse :: String -> Input
parse = map (runParser particle) . lines
  where
    particle = mkMotion <$ string "p=" <*> vector <*
        string ", v=" <*> vector <* string ", a=" <*> vector
    vector = Vector <$
        char '<' <*> int <* char ',' <*> int <* char ',' <*> int <* char '>'
    mkMotion p v a = Motion a v p

-- If asymptotic p = Motion da dv dp, then for large n,
-- Manhattan distance = da*(n*(n+1) `div` 2) + dv*n + dp
-- so we can compare these triples lexicographically.
asymptotic :: Particle -> Motion Int
asymptotic p =
    addMotions
        (asymptoticCoord (fmap xc p))
        (asymptoticCoord (fmap yc p))
        (asymptoticCoord (fmap zc p))

addMotions :: Motion Int -> Motion Int -> Motion Int -> Motion Int
addMotions (Motion ax vx px) (Motion ay vy py) (Motion az vz pz) =
    Motion (ax + ay + az) (vx + vy + vz) (px + py + pz)

-- make dominant component positive and apply its sign to the rest
asymptoticCoord :: Motion Int -> Motion Int
asymptoticCoord m = fmap (asymptoticSign m *) m

-- The sign of the dominant component,
-- which will be the sign of the position for large n
-- (-1 for negative, 0, or 1 for positive).
asymptoticSign :: Motion Int -> Int
asymptoticSign (Motion a v p)
  | a /= 0 = signum a
  | v /= 0 = signum v
  | otherwise = signum p

-- index of asymptotically closest particle
solve1 :: Input -> Int
solve1 = fst . minimumBy (comparing (asymptotic . snd)) . zip [0..]

testInput1 :: String
testInput1 = "\
    \p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>\n\
    \p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>\n"

tests1 :: [(String, Int)]
tests1 = [(testInput1, 0)]

-- Part Two

-- Collect indices of particles destroyed in collisions.  If a particle
-- is destroyed in a collision, later potential collisions with that
-- particle are ignored.  Thus we must consider potential collisions in
-- increasing time order.
collisions :: System -> Set Int
collisions ps =
    foldl addCollisions Set.empty $
    map (map snd) $
    groupSortOn fst $
    [(t, (n1, n2)) |
        (n1, p1):rest <- tails (zip [0..] ps), (n2, p2) <- rest,
        t <- collisionTimes p1 p2]

-- Given a set of already destroyed particles and a list of pairs of
-- potentially colliding particles at a point in time, determine which
-- are actual collisions (neither particle has been destroyed) and add
-- the colliding particles to the destroyed set.
addCollisions :: Set Int -> [(Int, Int)] -> Set Int
addCollisions s xys =
    Set.union s $ Set.fromList $ concat
        [[x, y] | (x, y) <- xys, not (Set.member x s) && not (Set.member y s)]

-- Times when the paths of two particles cross
collisionTimes :: Particle -> Particle -> [Int]
collisionTimes p1 p2 =
    componentCollisions (fmap xc p1) (fmap xc p2) `merge`
    componentCollisions (fmap yc p1) (fmap yc p2) `merge`
    componentCollisions (fmap zc p1) (fmap zc p2)

-- Intersection of two ordered lists
merge :: Ord a => [a] -> [a] -> [a]
merge [] _ = []
merge _ [] = []
merge (x:xs) (y:ys) = case compare x y of
    EQ -> x:merge xs ys
    LT -> merge xs (y:ys)
    GT -> merge (x:xs) ys

-- Integral times when two motions cross.
-- value at time n = a*(n*(n+1) `div` 2) + v*n + p
-- Doubling that yields a quadratic formula with integer coefficients.
componentCollisions :: Motion Int -> Motion Int -> [Int]
componentCollisions (Motion a1 v1 p1) (Motion a2 v2 p2) =
    filter (>= 0) (quadSolutions da (da+2*dv) (2*dp))
  where
    da = a1 - a2
    dv = v1 - v2
    dp = p1 - p2

-- Integral solutions to a quadratic equation
quadSolutions :: Int -> Int -> Int -> [Int]
quadSolutions a b c
  | a /= 0 = [q | k <- intSqrt (b*b - 4*a*c), q <- (k - b) `intDiv` (2*a)]
  | b /= 0 = (- c) `intDiv` b
  | c /= 0 = []
  | otherwise = [0..]

-- Exact division
intDiv :: Int -> Int -> [Int]
intDiv a b
  | a `mod` b == 0 = [a `div` b]
  | otherwise = []

-- Integral square roots
intSqrt :: Int -> [Int]
intSqrt n
  | n > 0 && k*k == n = [-k, k]
  | n == 0 = [0]
  | otherwise = []
  where
    k = floor (sqrt (fromIntegral n::Double))

solve2 :: Input -> Int
solve2 ps = length ps - Set.size (collisions ps)

testInput2 :: String
testInput2 = "\
    \p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>\n\
    \p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>\n\
    \p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>\n\
    \p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>\n"

tests2 :: [(String, Int)]
tests2 = [(testInput2, 1)]

main :: IO ()
main = do
    s <- readFile "input/20.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
