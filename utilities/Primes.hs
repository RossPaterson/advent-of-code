module Primes where

-- n = product [p^k | (p, k) <- primeFactors n]
-- p's primes in ascending order
primeFactors :: Int -> [(Int, Int)]
primeFactors = factorize primes 0
  where
    factorize _ 0 1 = []
    factorize (p:ps) k n
      | n `mod` p == 0 = factorize (p:ps) (k+1) (n `div` p)
      | k > 0 = (p, k):factorize ps 0 n
      | p*p > n = [(n, 1)]
      | otherwise = factorize ps 0 n

-- is n prime?
isPrime :: Int -> Bool
isPrime n = noFactors primes
  where
    noFactors (p:ps) = p*p > n || n `mod` p /= 0 && noFactors ps

-- infinite list of prime numbers
primes :: [Int]
primes = 2:filter isPrime [3,5..]
