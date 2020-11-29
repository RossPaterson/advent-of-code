module Main where

import Parser
import Utilities
import Control.Applicative
import Data.List

-- an nxn array of Booleans
data Grid = Grid Int [[Bool]]
    deriving (Eq, Show)

-- grid re-writing rule
data Rule = Rule Grid Grid
    deriving Show

type Input = [Rule]

parse :: String -> Input
parse = map (runParser rule) . lines

rule :: Parser Rule
rule = mkRule <$> grid <* string " => " <*> grid
  where
    mkRule lhs rhs = Rule lhs rhs

grid :: Parser Grid
grid = mkGrid <$> sepBy1 row (char '/')
  where
    mkGrid bss = Grid (length bss) bss
    row = many cell
    cell = True <$ char '#' <|> False <$ char '.'

showGrid :: Grid -> String
showGrid (Grid _ bss) = unlines (map (map showCell) bss)
  where
    showCell True = '#'
    showCell False = '.'

-- Splitting and reassembling the grid
-- The side of the square grid follows OEIS A000792 (https://oeis.org/A000792)

splitGrid :: Grid -> [[Grid]]
splitGrid (Grid n bss)
  | even n = subGrids 2 bss
  | otherwise = subGrids 3 bss

subGrids :: Int -> [[Bool]] -> [[Grid]]
subGrids n = map (map (Grid n) . transpose) . takes n . map (takes n)

reassemble :: [[Grid]] -> Grid
reassemble gss = foldr1 above (map (foldr1 beside) gss)

above :: Grid -> Grid -> Grid
above (Grid n1 bss1) (Grid n2 bss2) = Grid (n1+n2) (bss1 ++ bss2)

beside :: Grid -> Grid -> Grid
beside (Grid n1 bss1) (Grid n2 bss2) = Grid (n1+n2) (zipWith (++) bss1 bss2)

-- Rewrite the whole grid
step :: [Rule] -> Grid -> Grid
step rs = reassemble . map (map (rewrite rs)) . splitGrid

-- Rewrite a subgrid
-- The grid may be rotated and/or flipped to match the lhs, but the
-- rhs it is replaced with is unaffected.
-- Pre-computing a canonical rearrangement of the lhs would give a speedup
-- by a factor of a bit under 2.
rewrite :: [Rule] -> Grid -> Grid
rewrite rs g = head [rhs | Rule lhs rhs <- rs, elem lhs gs]
  where
    gs = arrangements g

-- ways of rotating or flipping the grid
arrangements :: Grid -> [Grid]
arrangements g =
    [arrangement |
        rotation <- take 4 (iterate rotate90 g),
        arrangement <- [rotation, flipGrid rotation]]

-- flip a grid around a vertical axis
flipGrid :: Grid -> Grid
flipGrid (Grid n bss) = Grid n (reverse bss)

-- rotate a grid 90 degrees clockwise
rotate90 :: Grid -> Grid
rotate90 (Grid n bss) = Grid n (map reverse (transpose bss))

-- Number of pixels that are on
onCount :: Grid -> Int
onCount (Grid _ bss) = sum (map (length . filter id) bss)

solve :: Int -> [Rule] -> Int
solve n rs = onCount (times n (step rs) start)

start :: Grid
start = runParser grid "\
    \.#./\
    \..#/\
    \###"

solve1 :: Input -> Int
solve1 = solve 5

testInput :: String
testInput =
    "../.# => ##./#../...\n\
    \.#./..#/### => #..#/..../..../#..#\n"

tests1 :: [((Int, String), Int)]
tests1 = [((2, testInput), 12)]

-- Part Two

solve2 :: Input -> Int
solve2 = solve 18

main :: IO ()
main = do
    s <- readFile "input/21.txt"
    let input = parse s
    putStr (unlines (failures "solve" (\(n, i) -> solve n (parse i)) tests1))
    print (solve1 input)
    print (solve2 input)
