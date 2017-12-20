module Main where

import Parser
import Utilities
import Control.Applicative
import Data.List
import Data.Ord

data Vector = Vector { xc :: !Int, yc :: !Int, zc :: !Int }
    deriving (Eq, Ord, Show)

-- Manhattan metric
distance :: Vector -> Int
distance (Vector x y z) = abs x + abs y + abs z

-- acceleration will dominate, then velocity, then position
data Particle a = Particle { acceleration :: a, velocity :: a, position :: a }
    deriving (Eq, Ord, Show)

instance Functor Particle where
    fmap f (Particle a v p) = Particle (f a) (f v) (f p)

type System = [Particle Vector]

type Input = System

parse :: String -> Input
parse = map (runParser particle) . lines
  where
    particle = mkParticle <$ string "p=" <*> vector <*
        string ", v=" <*> vector <* string ", a=" <*> vector
    vector = Vector <$
        char '<' <*> int <* char ',' <*> int <* char ',' <*> int <* char '>'
    mkParticle p v a = Particle a v p

-- If asymptotic p = Particle da dv dp, then for large n,
-- Manhattan distance = da*(n*(n+1) `div` 2) + dv*n + dp
-- so we can compare these triples lexicographically.
asymptotic :: Particle Vector -> Particle Int
asymptotic p =
    addParticles
        (asymptoticCoord (fmap xc p))
        (asymptoticCoord (fmap yc p))
        (asymptoticCoord (fmap zc p))

addParticles :: Particle Int -> Particle Int -> Particle Int -> Particle Int
addParticles (Particle ax vx px) (Particle ay vy py) (Particle az vz pz) =
    Particle (ax + ay + az) (vx + vy + vz) (px + py + pz)

-- make dominant component positive and apply its sign to the rest
asymptoticCoord :: Particle Int -> Particle Int
asymptoticCoord (Particle a v p)
  | a /= 0 = Particle (abs a) (v * signum a) (p * signum a)
  | v /= 0 = Particle 0 (abs v) (p * signum v)
  | otherwise = Particle 0 0 (abs p)

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

add :: Vector -> Vector -> Vector
add (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1+x2) (y1+y2) (z1+z2)

move :: Particle Vector -> Particle Vector
move (Particle a v p) = Particle a v' p'
  where
    v' = add v a
    p' = add p v'

removeCollisions :: System -> System
removeCollisions =
    map head . filter ((== 1) . length) .
        groupBy collide . sortBy (comparing position)
  where
    collide p1 p2 = position p1 == position p2

solve2 :: Input -> Int
solve2 = length . head . drop 1000 . iterate (map move . removeCollisions)

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
    s <- readFile "input20.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
