module Main where

import Parser
import Utilities

type Component = (Int, Int)

type Input = [Component]

parse :: String -> Input
parse = map (runParser component) . lines
  where
    component = (,) <$> nat <* char '/' <*> nat

strength :: [Component] -> Int
strength comps = sum [x+y | (x, y) <- comps]

bridges :: [Component] -> [[Component]]
bridges = bridgesFrom 0

bridgesFrom :: Int -> [Component] -> [[Component]]
bridgesFrom n comps =
    []:[(x, y):path |
        (e, rest) <- pick comps,
        (x, y) <- arrangements e,
        x == n,
        path <- bridgesFrom y rest]
  where
    arrangements (x, y)
      | x == y = [(x, y)]
      | otherwise = [(x, y), (y, x)]

solve1 :: Input -> Int
solve1 = maximum . map strength . bridges

testInput :: String
testInput = "0/2\n\
    \2/2\n\
    \2/3\n\
    \3/4\n\
    \3/5\n\
    \0/1\n\
    \10/1\n\
    \9/10\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 31)]

-- Part Two

measure :: [Component] -> (Int, Int)
measure bridge = (length bridge, strength bridge)

solve2 :: Input -> Int
solve2 = snd . maximum . map measure . bridges

tests2 :: [(String, Int)]
tests2 = [(testInput, 19)]

main :: IO ()
main = do
    s <- readFile "input/24.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
