module Main where

import Parser
import qualified Data.CyclicList as CL
import Utilities
import Control.Applicative
import Data.List
import Data.Monoid
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = (PlantString, Rules)
type Rules = Set PlantString
type PlantString = [Bool]

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
solve1 = sumPlants . getState 20

-- the state after n steps
getState :: Int -> (PlantString, Rules) -> Plants
getState n (bs, gs) = addOffset totalOffset lastState
  where
    generations = iterate (growPlants gs . present) (mkPlants 0 bs)
    totalOffset = sum (map offset (take n generations))
    lastState = generations!!n

data Plants = Plants {
    offset :: Int, -- offset of leftmost plant from previous generation
    present :: PlantString -- plant present (first and last are True)
    }
  deriving (Show, Eq, Ord)

-- add an offset to a set of plants
addOffset :: Int -> Plants -> Plants
addOffset off (Plants pos bs) = Plants (off+pos) bs

showPlants :: Plants -> String
showPlants (Plants pos bs) =
   "(" ++ show pos ++ ") " ++ [if b then '#' else '.' | b <- bs]

-- sum of positions of pots containing a plant
sumPlants :: Plants -> Int
sumPlants (Plants pos bs) = sum [n | (n, b) <- zip [pos..] bs, b]

-- normalize a set of plants, by removing empties from the ends
mkPlants :: Int -> PlantString -> Plants
mkPlants pos bs = Plants { offset = pos + length front, present = back }
  where
    (front, back) = span not $ reverse $ dropWhile not $ reverse bs

-- one growth step
growPlants :: Rules -> PlantString -> Plants
growPlants gcs bs =
    mkPlants (-2) $ map (flip Set.member gcs) $ sublists 5 False bs

-- sublists of length n, padding with v
sublists :: Int -> a -> [a] -> [[a]]
sublists n v bs =
    take (len + 2*n - 2) $
    map (take n) $ tails $ replicate (n-1) v ++ bs ++ repeat v
  where
    len = length bs

testInput :: String
testInput =
    "initial state: #..#.#..##......###...###\n\
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

solve2 :: Input -> Int
solve2 = sumPlants . getStateRL 50000000000

-- same as getState, but using a repeating list
getStateRL :: Int -> (PlantString, Rules) -> Plants
getStateRL n (bs, gs) = addOffset totalOffset lastState
  where
    generations = CL.iterate (growPlants gs . present) (mkPlants 0 bs)
    totalOffset = getSum (CL.foldMapTake (Sum . offset) n generations)
    lastState = CL.elementAt n generations

main :: IO ()
main = do
    s <- readFile "input/12.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    print (solve2 input)
