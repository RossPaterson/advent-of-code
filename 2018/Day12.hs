module Main where

import Parser
import Utilities
import Control.Applicative
import Data.Functor
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = ([Bool], Set [Bool])

parse :: String -> Input
parse s = (s0, rules)
  where
    ls = lines s
    s0 = runParser initial (head ls)
    initial = string "initial state: " *> plants
    rules = Set.fromList [bs | (bs, b) <- map (runParser rule) (drop 2 ls), b]
    rule = (,) <$> plants <* string " => " <*> plant
    plants = some plant
    plant = True <$ char '#' <|> False <$ char '.'

-- Part One

solve1 :: Input -> Int
solve1 = sumPlants . (!!gens1) . generations

gens1 :: Int
gens1 = 20

data Plants = Plants {
    offset :: Int, -- position of leftmost plant
    present :: [Bool] -- plant present (first and last are True)
    }
  deriving Show

showPlants :: Plants -> String
showPlants (Plants pos bs) =
   "(" ++ show pos ++ ") " ++ [if b then '#' else '.' | b <- bs]

-- sum of positions of pots containing a plant
sumPlants :: Plants -> Int
sumPlants (Plants pos bs) = sum [n | (n, b) <- zip [pos..] bs, b]

-- sequence of generations from a start point
generations :: Input -> [Plants]
generations (bs, gs) = iterate (growPlants gs) (mkPlants 0 bs)

-- normalize a set of plants
mkPlants :: Int -> [Bool] -> Plants
mkPlants pos bs = Plants { offset = pos + length front, present = back }
  where
    (front, back) = span not $ reverse $ dropWhile not $ reverse bs

-- one growth step
growPlants :: Set [Bool] -> Plants -> Plants
growPlants gcs (Plants pos bs) =
    mkPlants (pos-2) $ map (flip Set.member gcs) $ sublists 5 False bs

-- sublists of n, padding with v
sublists :: Int -> a -> [a] -> [[a]]
sublists n v bs =
    take (len + 2*n - 2) $
    map (take n) $ tails $ replicate (n-1) v ++ bs ++ repeat v
  where
    len = length bs

testInput = "\
\initial state: #..#.#..##......###...###\n\
\\n\
\...## => #\n\
\..#.. => #\n\
\.#... => #\n\
\.#.#. => #\n\
\.#.## => #\n\
\.##.. => #\n\
\.#### => #\n\
\#.#.# => #\n\
\#.### => #\n\
\##.#. => #\n\
\##.## => #\n\
\###.. => #\n\
\###.# => #\n\
\####. => #\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 325)]

-- Part Two

solve2 :: Input -> Integer
solve2 p = fromIntegral b + fromIntegral a * gens2
  where
    (a, b) = linearCoeffs (generations p)

gens2 :: Integer
gens2 = 50000000000

-- Assuming that the pattern of plants becomes fixed after a certain point,
-- the sums of positions will grow linearly as a*n+b thereafter.
-- This function then returns these coefficients (a, b).
linearCoeffs :: [Plants] -> (Int, Int)
linearCoeffs gs = (a, b)
  where
    (front, (ps1@(Plants pos1 bs1), ps2@(Plants pos2 bs2)):rest) =
        span (not . uncurry (same present)) $ zip gs (tail gs)
    n = length front
    a = (pos2 - pos1) * length (filter id bs1)
    b = sumPlants ps1 - n*a

main :: IO ()
main = do
    s <- readFile "input12.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    print (solve2 input)
