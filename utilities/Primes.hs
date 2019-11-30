-- | Number theory
module Primes where

-- | Factorization of the argument:
--
-- prop> n = product [p^k | (p, k) <- primeFactors n]
--
-- where the @p@'s are primes in ascending order, and each @k@ is positive.
primeFactors :: Int -> [(Int, Int)]
primeFactors = factorize primes
  where
    factorize (p:ps) n
      | n == 1 = []
      | n `mod` p == 0 = primePower 1 (n `div` p)
      | p*p > n = [(n, 1)]
      | otherwise = factorize ps n
      where
        primePower k m
          | m `mod` p == 0 = primePower (k+1) (m `div` p)
          | otherwise = (p, k):factorize ps m

-- | Is @n@ prime?
isPrime :: Int -> Bool
isPrime n = null [p | p <- takeWhile small primes, n `mod` p == 0]
  where
    small p = p*p <= n

-- | The infinite list of prime numbers
primes :: [Int]
primes = 2:filter isPrime [3,5..]

-- | The sum of all the factors of @n@
sumOfFactors :: Int -> Int
sumOfFactors n = product [(p^(k+1) - 1) `div` (p-1) | (p, k) <- primeFactors n]
