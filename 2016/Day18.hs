module Main where

import Utilities
import Data.Char

type Row = [Bool]
type Input = Row

parse :: String -> Row
parse = map (== '^') . filter (not . isSpace)

showRow :: Row -> String
showRow bs = [if b then '^' else '.' | b <- bs]

-- the rules are equivalent to trap <=> left /= right
nextRow :: Row -> Row
nextRow r = zipWith (/=) (tail r ++ [False]) (False:r)

numSafe :: [Row] -> Int
numSafe = sum . map (length . filter not)

tests1 :: [((Int, String), Int)]
tests1 = [
    ((3, "..^^."), 6),
    ((10, ".^^.^.^^^^"), 38)]

safeCount :: Int -> Input -> Int
safeCount n = numSafe . take n . iterate nextRow

solve1 :: Input -> Int
solve1 = safeCount 40

-- Part Two --

solve2 :: Input -> Int
solve2 = safeCount 400000

main :: IO ()
main = do
    s <- readFile "input/18.txt"
    let input = parse s
    putStr (unlines (failures "safeCount" (uncurry safeCount . fmap parse) tests1))
    print (solve1 input)
    print (solve2 input)
