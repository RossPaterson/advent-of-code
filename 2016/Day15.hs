module Main where

import Number
import Parser
import Data.List
import Data.Ord

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

-- Mod i n denotes position i in n-cycle.  0 <= i < n
mkMod :: Int -> Int -> Mod
mkMod i n = Mod (toInteger (i `mod` n)) (toInteger n)

positionToMod :: Disc -> Mod
positionToMod (Disc n npos t0 pos0) = mkMod (t0 - n - pos0) npos

-- the sort means the largest cycle is trivial
chineseRemainder :: [Mod] -> Mod
chineseRemainder = foldr1 match . sortBy (comparing cycle_size)
  where
    -- Mod k (p*n) such that k == i (mod p) and k == j (mod n)
    -- assumes gcd p n == 1
    match (Mod i p) (Mod j n) = Mod ((i*y*n + j*x*p) `mod` (p*n)) (p*n)
      where
        (x, y) = bezout p n -- x*p + y*n = 1

solve1 :: Input -> Integer
solve1 = remainder . chineseRemainder . map positionToMod

testInput :: String
testInput =
    "Disc #1 has 5 positions; at time=0, it is at position 4.\n\
    \Disc #2 has 2 positions; at time=0, it is at position 1.\n"

-- Part Two --

addDisc :: Int -> Int -> [Disc] -> [Disc]
addDisc npos pos0 discs = discs ++ [Disc (length discs + 1) npos 0 pos0]

solve2 :: Input -> Integer
solve2 = solve1 . addDisc 11 0

main :: IO ()
main = do
    s <- readFile "input/15.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
