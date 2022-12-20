module Main where

import Utilities
import Parser
import Control.Applicative
import Data.Char
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import Data.Tree

-- Input processing

type Input = Blueprints
type Blueprints = [(Int, Blueprint)]
type Blueprint = Map Material [(Int, Material)]
data Material = Ore | Clay | Obsidian | Geode
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

parse :: String -> Input
parse = map (runParser blueprint) . lines
  where
    blueprint = (,) <$ string "Blueprint " <*> nat <* char ':' <*> recipes
    recipes = Map.fromList <$> some recipe
    recipe = (,) <$ string " Each " <*> name <* string " robot costs " <*>
        costs <* char '.'
    costs = sepBy1 cost (string " and ")
    cost = (,) <$> nat <* space <*> name
    name = material <$> some letter
    material (c:cs) = read (toUpper c:cs)
    material [] = error "empty material name"

-- Part One

type Collection = Map Material Int

get :: Material -> Collection -> Int
get m = Map.findWithDefault 0 m

-- States of the system

data State = State {
    clock :: Int,
    materials :: Collection,
    robots :: Collection
    }
    deriving (Eq, Ord, Show)

initState :: State
initState = State {
    clock = 0,
    materials = Map.empty,
    robots = Map.singleton Ore 1
    }

builds :: Blueprint -> State -> [State]
builds bp s =
    [build bp s m |
        m <- allValues,
        m == Ore || get (pred m) (robots s) > 0,
        -- We don't need more than 4 ore per turn for any of the blueprints
        m /= Ore || get m (robots s) < 4]

build :: Blueprint -> State -> Material -> State
build bp s r = State {
        clock = clock s + dt,
        materials = foldr spend new_materials requirements,
        robots = Map.insertWith (+) r 1 (robots s)
    }
  where
    new_materials = Map.unionWith (+) (materials s) (Map.map (*dt) (robots s))
    -- how many steps we have to wait until a new robot r can be ready
    dt = foldr max 0
        [div_round_up (q - get m (materials s)) (get m (robots s)) |
            (q, m) <- requirements] + 1
    requirements = bp ! r
    div_round_up a b = (a + b - 1) `div` b

spend :: (Int, Material) -> Collection -> Collection
spend (q, m) = Map.adjust (subtract q) m

buildTree :: Blueprint -> Tree State
buildTree bp = iterateTree (builds bp) initState

-- number of geodes produced up to time n if no further robots made
value :: Int -> State -> Int
value time_limit s = get Geode (materials s) + time_left*get Geode (robots s)
  where
    time_left = time_limit - clock s

-- Number of geodes produced up to the time limit if we build a geode
-- robot every tick from the clock until then.  This is a bound on the
-- number of geodes that can be produced up to the time limit.
value_bound :: Int -> State -> Int
value_bound time_limit s =
    value time_limit s + time_left*(time_left-1) `div` 2
  where
    time_left = time_limit - clock s

mostGeodes :: Int -> Blueprint -> Int
mostGeodes time_limit =
    maximumDF (value time_limit) (value_bound time_limit) .
        takeWhileTree (\ s -> clock s <= time_limit) . buildTree

solve1 :: Input -> Int
solve1 input = sum [n*mostGeodes 24 bp | (n, bp) <- input]

testInput :: String
testInput = "\
    \Blueprint 1:\
    \ Each ore robot costs 4 ore.\
    \ Each clay robot costs 2 ore.\
    \ Each obsidian robot costs 3 ore and 14 clay.\
    \ Each geode robot costs 2 ore and 7 obsidian.\
    \\n\
    \Blueprint 2:\
    \ Each ore robot costs 2 ore.\
    \ Each clay robot costs 3 ore.\
    \ Each obsidian robot costs 3 ore and 8 clay.\
    \ Each geode robot costs 3 ore and 12 obsidian."

tests1 :: [(String, Int)]
tests1 = [(testInput, 33)]

-- Part Two

solve2 :: Input -> Int
solve2 input = product [mostGeodes 32 bp | (_, bp) <- take 3 input]

tests2 :: [(String, Int)]
tests2 = [(testInput, 56*62)]

main :: IO ()
main = do
    s <- readFile "input/19.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
