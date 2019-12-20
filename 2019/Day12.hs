module Main where

import Utilities
import Parser
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = [Point Int]

type Point a = (a, a, a)

zipPoint :: (a -> b -> c) -> Point a -> Point b -> Point c
zipPoint f (x1, y1, z1) (x2, y2, z2) = (f x1 x2, f y1 y2, f z1 z2)

parse :: String -> Input
parse = map (runParser point) . lines
  where
    point = (,,) <$ string "<x=" <*> int <* string ", y=" <*> int <* string ", z=" <*> int <* char '>'

-- Part One

data Body a = Body {
    position :: a,
    velocity :: a
    }
    deriving (Eq, Ord, Show)

norm :: Point Int -> Int
norm (x, y, z) = abs x + abs y + abs z

zero :: Point Int
zero = (0, 0, 0)

plus :: Point Int -> Point Int -> Point Int
plus = zipPoint (+)

gravity :: Point Int -> Point Int -> Point Int
gravity = zipPoint gravity1

gravity1 :: Int -> Int -> Int
gravity1 a b = signum (b - a)

stationary :: Point Int -> Body (Point Int)
stationary p = Body p zero

move :: Body (Point Int) -> Body (Point Int)
move (Body p v) = Body (p `plus` v) v

-- update velocities using gravity
-- (no need to exclude gravity of the body on itself, because that is zero)
attract :: [Body (Point Int)] -> [Body (Point Int)]
attract bs =
    [Body p (foldr plus v (map (gravity p . position) bs)) | Body p v <- bs]

step :: [Body (Point Int)] -> [Body (Point Int)]
step = map move . attract

energy :: Body (Point Int) -> Int
energy (Body p v) = norm p * norm v

energyAfter :: Int -> [Point Int] -> Int
energyAfter n = sum . map energy . times n step . map stationary

solve1 :: Input -> Int
solve1 = energyAfter 1000

testInput1 :: String
testInput1 = "\
    \<x=-1, y=0, z=2>\n\
    \<x=2, y=-10, z=-7>\n\
    \<x=4, y=-8, z=8>\n\
    \<x=3, y=5, z=-1>\n"

testInput2 :: String
testInput2 = "\
    \<x=-8, y=-10, z=0>\n\
    \<x=5, y=5, z=10>\n\
    \<x=2, y=-7, z=3>\n\
    \<x=9, y=-8, z=-3>\n"

tests1 :: [((String, Int), Int)]
tests1 = [((testInput1, 10), 179), ((testInput2, 100), 1940)]

-- Part Two

-- Key insight: all calculations affect each coordinate independently,
-- so find how long it takes each the x-parts to repeat, and so on.
-- Also useful: steps are invertible, so cycle starts at initial state.

-- Other properties (not used directly, but help with repetition):
-- * total velocity is zero
-- * total position is constant

transPoints :: [Point a] -> Point [a]
transPoints ps = (map xcoord ps, map ycoord ps, map zcoord ps)
  where
    xcoord (x, _, _) = x
    ycoord (_, y, _) = y
    zcoord (_, _, z) = z

-- Scalar forms of the functions in part one

stationary1 :: Int -> Body Int
stationary1 p = Body p 0

move1 :: Body Int -> Body Int
move1 (Body p v) = Body (p + v) v

attract1 :: [Body Int] -> [Body Int]
attract1 bs =
    [Body p (foldr (+) v (map (gravity1 p . position) bs)) | Body p v <- bs]

step1 :: [Body Int] -> [Body Int]
step1 = map move1 . attract1

-- number of elements of a list before the first repetition
num_uniq :: Ord a => [a] -> Int
num_uniq xs = length $ takeWhile not $ zipWith Set.member xs (init_sets xs)

-- same as map Set.fromList (inits xs)
init_sets :: Ord a => [a] -> [Set a]
init_sets = scanl (flip Set.insert) Set.empty

-- time until a repetition of the first value
repeatTime1 :: [Int] -> Integer
repeatTime1 = toInteger . num_uniq . iterate step1 . map stationary1

repeatTime :: [Point Int] -> Integer
repeatTime ps = rx `lcm` ry `lcm` rz
  where
    (xs, ys, zs) = transPoints ps
    rx = repeatTime1 xs
    ry = repeatTime1 ys
    rz = repeatTime1 zs

solve2 :: Input -> Integer
solve2 = repeatTime

tests2 :: [(String, Integer)]
tests2 = [(testInput1, 2772), (testInput2, 4686774924)]

main :: IO ()
main = do
    s <- readFile "input/12.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (\(t, n) -> energyAfter n (parse t)) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
