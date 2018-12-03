module Main where

import Utilities

type Bits = [Bool]

stringToBits :: String -> Bits
stringToBits = map (== '1')

bitsToString :: Bits -> String
bitsToString bs = [if b then '1' else '0' | b <- bs]

expansion :: Bits -> Bits
expansion bs = bs ++ concat (zipWith (:) bits (cycle [bs', bs]))
  where
    bs' = reverse (map not bs)

-- A038189 (ones in positions A091067)
bits :: Bits
bits = [oddPart n `mod` 4 == 3 | n <- [1..]]

oddPart :: Int -> Int
oddPart = fst . twos

-- (n, k) such that n is zero or odd and n*2^k == x
twos :: Int -> (Int, Int)
twos 0 = (0, 0)
twos x = splitPower 0 x
  where
    splitPower k n
      | k `seq` odd n = (n, k)
      | even n = splitPower (k+1) (n `div` 2)

solve :: Int -> String -> String
solve size = bitsToString .
    take n . times k (pairWith (==)) .  expansion . stringToBits
  where
    (n, k) = twos size

input :: String
input = "01111010110010011"

main :: IO ()
main = do
    putStrLn (solve 272 input)
    putStrLn (solve 35651584 input)
