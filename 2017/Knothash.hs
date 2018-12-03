module Knothash(hashRounds, knothash) where

import Utilities
import Data.Bits
import Data.Char

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

-- apply n rounds of the list of lengths to the initial state
hashRounds :: Int -> Int -> [Int] -> [Int]
hashRounds n size lengths = numbers (times n round (initState size))
  where
    round s = foldl step s lengths

knothash :: String -> [Int]
knothash s = dense (hashRounds 64 256 lengths)
  where
    lengths = map ord (filter (/= '\n') s) ++ [17, 31, 73, 47, 23]
    dense = map (foldr1 xor) . groups 16
