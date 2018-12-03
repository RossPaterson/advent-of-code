module Main where

-- n = product [p^k | (p, k) <- primeFactors n]
-- p's primes in ascending order
primeFactors :: Int -> [(Int, Int)]
primeFactors = factorize 2 0
  where
    factorize _ 0 1 = []
    factorize p k n
      | n `mod` p == 0 = factorize p (k+1) (n `div` p)
      | k > 0 = (p, k):factorize (p+1) 0 n
      | p*p > n = [(n, 1)]
      | otherwise = factorize (p+1) 0 n

sumOfFactors :: Int -> Int
sumOfFactors n = product [(p^(k+1) - 1) `div` (p-1) | (p, k) <- primeFactors n]

input :: Int
input = 29000000

solve1 :: Int
solve1 = head [n | n <- [1..], 10*sumOfFactors n >= input]

-- Part Two --

bigFactors :: Int -> [Int]
bigFactors n = [n `div` k | k <- [1..min n 50], n `mod` k == 0]

solve2 :: Int
solve2 = head [n | n <- [1..], 11*sum (bigFactors n) >= input]

main :: IO ()
main = do
    print solve1
    print solve2
