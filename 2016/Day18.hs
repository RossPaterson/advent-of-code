module Main where

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

test :: String -> Int -> IO ()
test s n = do
    putStrLn $ unlines $ map showRow rows
    putStrLn $ "There are " ++ show (numSafe rows) ++ " safe tiles"
  where
    rows = take n $ iterate nextRow $ parse s

test1 = test "..^^." 3
test2 = test ".^^.^.^^^^" 10

solve1 :: Input -> Int
solve1 = numSafe . take 40 . iterate nextRow

-- Part Two --

solve2 :: Input -> Int
solve2 = numSafe . take 400000 . iterate nextRow

main :: IO ()
main = do
    s <- readFile "input18.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
