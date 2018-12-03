module Main where

import Knothash
import Utilities
import Data.Bits
import Data.Char
import Data.List
import Numeric

type Input = [Int]

parse :: String -> Input
parse cs = map read (words [if c == ',' then ' ' else c | c <- cs, c /= '\n'])

hash1 :: Int -> [Int] -> [Int]
hash1 = hashRounds 1

solve1 :: Input -> Int
solve1 = product . take 2 . hash1 256

tests1 :: [((Int, [Int]), [Int])]
tests1 = [((5, [3, 4, 1, 5]), [3, 4, 2, 1, 0])]

-- Part Two

solve2 :: String -> String
solve2 = hex . knothash 
  where
    hex = concatMap hexDigit
    hexDigit n
      | n < 16 = "0" ++ showHex n ""
      | otherwise = showHex n ""

tests2 :: [(String, String)]
tests2 = [
    ("", "a2582a3a0e66e6e86e3812dcb672a272"),
    ("AoC 2017", "33efeb34ea91902bb2f59c9920caa6cd"),
    ("1,2,3", "3efbe78a8d82f29979031a4aa0b16a9d"),
    ("1,2,4", "63960835bcdc130f0b66d7ff4f6a5a8e")]

main :: IO ()
main = do
    s <- readFile "input10.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (uncurry hash1) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" solve2 tests2))
    putStrLn (solve2 s)
