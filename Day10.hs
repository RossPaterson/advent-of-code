module Main where

import Utilities
import Data.Bits
import Data.Char
import Data.List
import Numeric

type Input = [Int]

parse :: String -> Input
parse cs = map read (words [if c == ',' then ' ' else c | c <- cs, c /= '\n'])

-- the list of numbers is rotated by position
data State = State { list :: [Int], position :: Int, skipSize :: Int }
    deriving Show

-- the list of numbers in their original positions
numbers :: State -> [Int]
numbers (State ns pos _) = back ++ front
  where
    (front, back) = splitAt (length ns - pos) ns

initState :: Int -> State
initState size = State [0..size-1] 0 0

-- apply f to the next n elements of the ring, and advance to the end of that
advance :: Int -> ([Int] -> [Int]) -> State -> State
advance n f (State ns pos skip) = State ns' pos' skip
  where
    (front, back) = splitAt n ns
    ns' = back ++ f front
    pos' = (pos + n) `mod` size
    size = length ns

incrSkip :: State -> State
incrSkip (State ns pos skip) = State ns pos ((skip+1) `mod` length ns)

{-
One step of the hash:
- Reverse the order of that length of elements in the list, starting
  with the element at the current position.
- Move the current position forward by that length plus the skip size.
- Increase the skip size by one.
-}
step :: State -> Int -> State
step s len = incrSkip $ advance (skipSize s) id $ advance len reverse s

hash1 :: Int -> [Int] -> [Int]
hash1 size = numbers . foldl step (initState size)

solve1 :: Input -> Int
solve1 = product . take 2 . hash1 256

tests1 :: [((Int, [Int]), [Int])]
tests1 = [((5, [3, 4, 1, 5]), [3, 4, 2, 1, 0])]

-- Part Two

solve2 :: String -> String
solve2 s = hex $ dense $ numbers $ times 64 round $ initState 256
  where
    round s = foldl step s lengths
    lengths = map ord (filter (/= '\n') s) ++ [17, 31, 73, 47, 23]
    dense = map (foldr1 xor) . groups 16
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
