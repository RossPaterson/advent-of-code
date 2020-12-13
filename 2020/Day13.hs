module Main where

import Utilities
import Number
import Parser
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Ord

-- Input processing

type Input = (Int, [Maybe Int])

parse :: String -> Input
parse s = (read (head ls), runParser busses (ls!!1))
  where
    busses = sepBy1 bus (char ',')
    bus = Nothing <$ char 'x' <|> Just <$> nat
    ls = lines s

-- Part One

solve1 :: Input -> Int
solve1 (n, bs) =
    uncurry (*) $
    minimumBy (comparing fst) $
    [((- n) `mod` t, t) | t <- catMaybes bs]

testInput1 :: String
testInput1 = "\
    \939\n\
    \7,13,x,x,59,x,31,19"

tests1 :: [(String, Int)]
tests1 = [(testInput1, 295)]

-- Part Two

entries :: [Maybe a] -> [(Int, a)]
entries mvs = [(n, v) | (n, Just v) <- zip [0..] mvs]

solve2 :: Input -> Int
solve2 (_, ps) = chineseRemainder [((- i) `mod` n, n) | (i, n) <- entries ps]

tests2 :: [(String, Int)]
tests2 = [
    (testInput1, 1068781),
    ("0\n17,x,13,19", 3417),
    ("0\n67,7,59,61", 754018),
    ("0\n67,x,7,59,61", 779210),
    ("0\n67,7,x,59,61", 1261476),
    ("0\n1789,37,47,1889", 1202161486)]

main :: IO ()
main = do
    s <- readFile "input/13.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
