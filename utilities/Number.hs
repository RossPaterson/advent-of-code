-- | Elementary number theory
module Number where

-- | Factorization of the argument:
--
-- prop> n = product [p^k | (p, k) <- primeFactors n]
--
-- where the @p@'s are primes in ascending order, and each @k@ is positive.
primeFactors :: Int -> [(Int, Int)]
primeFactors = factorize primes
  where
    factorize [] _ = error "run out of primes"
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
isPrime n = and [n `mod` p /= 0 | p <- takeWhile small primes]
  where
    small p = p*p <= n

-- | The infinite list of prime numbers
primes :: [Int]
primes = 2:filter isPrime [3,5..]

-- | The number of factors of @n@
numberOfFactors :: Int -> Int
numberOfFactors n = product [k+1 | (_, k) <- primeFactors n]

-- | The sum of all the factors of @n@
sumOfFactors :: Int -> Int
sumOfFactors n = product [(p^(k+1) - 1) `div` (p-1) | (p, k) <- primeFactors n]

-- | @'bezout' a b = (x, y)@ such that @a*x + b*y = 'gcd' a b@
-- (Bézout's identity).
--
-- In particular, if @a@ and @b@ are coprime, @y@ is the multiplicative
-- inverse of @b@ modulo @a@.
bezout :: Integral a => a -> a -> (a, a)
bezout a b = (signum a * approx 1 0 qs, signum b * approx 0 1 qs)
  where
    qs = quotients (abs a) (abs b)

    quotients _ 0 = []
    quotients x y = q:quotients y r
      where
        (q, r) = quotRem x y

    approx x _ [] = x
    approx x y (c:cs) = approx y (x - c*y) cs
