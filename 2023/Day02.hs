module Main where

import Parser
import Utilities
import Control.Applicative
import Data.Tuple
import Data.Map (Map)
import qualified Data.Map as Map

-- Input processing

type Input = [Game]

data Game = Game Int [CubeSet]
    deriving (Show)

type CubeSet = Map Colour Int

data Colour = Red | Green | Blue
    deriving (Eq, Ord, Show)

parse :: String -> Input
parse = map (runParser game) . lines
  where
    game = Game <$ string "Game " <*> nat <* string ": " <*> (cubeset `sepBy1` string "; ")
    cubeset = (Map.fromList . map swap) <$> cubes `sepBy1` string ", "
    cubes = (,) <$> nat <* char ' ' <*> colour
    colour =
        Red <$ string "red" <|>
        Green <$ string "green" <|>
        Blue <$ string "blue"

-- Part One

bag :: CubeSet
bag = Map.fromList [(Red, 12), (Green, 13), (Blue, 14)]

-- the set has no more of each colour than the bag
possible :: CubeSet -> Bool
possible cs = and (Map.intersectionWith (<=) cs bag)

solve1 :: Input -> Int
solve1 gs = sum [n | Game n sets <- gs, all possible sets]

testInput :: String
testInput = "\
    \Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
    \Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
    \Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
    \Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
    \Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 8)]

-- Part Two

emptySet :: CubeSet
emptySet = Map.fromList [(Red, 0), (Green, 0), (Blue, 0)]

-- the smallest bag that makes this game possible
minBag :: Game -> CubeSet
minBag (Game _ sets) = foldr (Map.unionWith max) emptySet sets

solve2 :: Input -> Int
solve2 = sum . map (product . minBag)

tests2 :: [(String, Int)]
tests2 = [(testInput, 2286)]

main :: IO ()
main = do
    s <- readFile "input/02.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
