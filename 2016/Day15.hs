module Main where

import Number
import Parser
import Utilities

data Disc = Disc {
    disc_number :: Int,
    num_positions :: Int, -- assumed to be distinct primes
    start_time :: Int,
    start_position :: Int }
  deriving Show
type Input = [Disc]

parse :: String -> Input
parse = map (runParser disc) . lines
  where
    disc = Disc <$ string "Disc #" <*> nat <* string " has " <*> nat
        <* string " positions; at time=" <*> nat <*
        string ", it is at position " <*> nat <* string "."

-- position of disc when capsule released at time t arrives
position :: Int -> Disc -> Int
position t (Disc n npos t0 pos0) = (t - t0 + n + pos0) `mod` npos

data Mod = Mod { remainder :: Integer, cycle_size :: Integer }
  deriving Show

-- position t d == 0 <=> t == t0 - n - pos0 (mod npos)

-- (i, n) denotes position i in n-cycle.  0 <= i < n
positionToMod :: Disc -> (Int, Int)
positionToMod (Disc n npos t0 pos0) = ((t0 - n - pos0) `mod` npos, npos)

solve1 :: Input -> Int
solve1 = chineseRemainder . map positionToMod

testInput :: String
testInput =
    "Disc #1 has 5 positions; at time=0, it is at position 4.\n\
    \Disc #2 has 2 positions; at time=0, it is at position 1.\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 5)]

-- Part Two --

addDisc :: Int -> Int -> [Disc] -> [Disc]
addDisc npos pos0 discs = discs ++ [Disc (length discs + 1) npos 0 pos0]

solve2 :: Input -> Int
solve2 = solve1 . addDisc 11 0

main :: IO ()
main = do
    s <- readFile "input/15.txt"
    let input = parse s
    print (solve1 input)
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve2 input)
