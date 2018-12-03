module Main where

import Parser
import Utilities
import Control.Applicative
import Data.Bits
import Data.List

data Grid = Grid Int [[Bool]]
    deriving (Eq, Show)
data Rule = Rule CompactGrid Grid Grid
    deriving Show

type Input = [Rule]

parse :: String -> Input
parse = map (runParser rule) . lines

rule :: Parser Rule
rule = mkRule <$> grid <* string " => " <*> grid
  where
    mkRule lhs rhs = Rule (normalize lhs) lhs rhs

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

-- Reduced form of a grid

data CompactGrid = Grid2 Int | Grid3 Int
    deriving (Eq, Ord, Show)

toCompact :: Grid -> CompactGrid
toCompact (Grid n bss) = (if n == 2 then Grid2 else Grid3) $
    foldr (addRow n) 0 (map (foldr addBit 0) bss)
  where
    addRow n x y = x .|. shift y n
    addBit True x = shift x 1 .|. bit 0
    addBit False x = shift x 1

fromCompact :: CompactGrid -> Grid
fromCompact (Grid2 x) = Grid 2 (getRows 2 x)
fromCompact (Grid3 x) = Grid 3 (getRows 3 x)

getRows :: Int -> Int -> [[Bool]]
getRows n x = [[testBit x (r*n + c) | c <- [0..n-1]] | r <- [0..n-1]]

-- Represent a left-hand side with a canonical compact from

normalize :: Grid -> CompactGrid
normalize = minimum . map toCompact . arrangements

arrangements :: Grid -> [Grid]
arrangements g = [f (times n rotate90 g) | f <- [id, flipGrid], n <- [0..3]]

flipGrid :: Grid -> Grid
flipGrid (Grid n bss) = Grid n (reverse bss)

rotate90 :: Grid -> Grid
rotate90 (Grid n bss) = Grid n (map reverse (transpose bss))

-- Splitting and reassembling the grid

splitGrid :: Grid -> [[Grid]]
splitGrid (Grid n bss)
  | even n = subGrids 2 bss
  | otherwise = subGrids 3 bss

subGrids :: Int -> [[Bool]] -> [[Grid]]
subGrids n = map (map (Grid n) . transpose) . groups n . map (groups n)

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
rewrite :: [Rule] -> Grid -> Grid
rewrite rs g = head [rhs | Rule nlhs _ rhs <- rs, nlhs == ng]
  where
    ng = normalize g

-- Number of pixels that are on
onCount :: Grid -> Int
onCount (Grid _ bss) = sum (map (length . filter id) bss)

-- 
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
testInput = "\
    \../.# => ##./#../...\n\
    \.#./..#/### => #..#/..../..../#..#\n"

tests1 :: [((Int, String), Int)]
tests1 = [((2, testInput), 12)]

-- Part Two

solve2 :: Input -> Int
solve2 = solve 18

main :: IO ()
main = do
    s <- readFile "input21.txt"
    let input = parse s
    putStr (unlines (failures "solve" (\(n, i) -> solve n (parse i)) tests1))
    print (solve1 input)
    print (solve2 input)
