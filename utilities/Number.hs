-- | Elementary number theory
module Number(
    -- * Prime numbers
    primes,
    isPrime,
    -- * Factors
    primeFactors,
    numberOfDivisors,
    sumOfDivisors,
    sumOfDivisorPowers,
    -- * Bézout's identity
    bezout,
    -- * Quadratic residues
    legendre,
    sqrtMod
    ) where

import Data.List

-- | The infinite list of prime numbers
primes :: [Int]
primes = 2:filter isPrime [3,5..]

-- | Is @n@ prime?
isPrime :: Int -> Bool
isPrime n = and [n `mod` p /= 0 | p <- takeWhile small primes]
  where
    small p = p*p <= n

-- | Factorization of the argument:
--
-- prop> n = product [p^k | (p, k) <- primeFactors n]
--
-- where the @p@'s are primes in ascending order, and each @k@ is positive
-- (Fundamental Theorem of Arithmetic).
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

-- | The number of divisors of @n@
numberOfDivisors :: Int -> Int
numberOfDivisors n = product [k+1 | (_, k) <- primeFactors n]

-- | The sum of all the divisors of @n@
sumOfDivisors :: Int -> Integer
sumOfDivisors = sumOfDivisorPowers 1

-- | @'sumOfDivisorPowers' m n@ is the sum of @m@th powers all the
-- divisors of @n@.
sumOfDivisorPowers :: Int -> Int -> Integer
sumOfDivisorPowers m n
  | m <= 0 = toInteger (numberOfDivisors n)
  | otherwise =
    product [sumPowers (toInteger p^m) k | (p, k) <- primeFactors n]

-- @'sumPowers' x k = x^k + ... + x + 1@
sumPowers :: Integer -> Int -> Integer
sumPowers x k = fromInteger ((x^(k+1) - 1) `div` (x-1))

-- | @'bezout' a b = (x, y)@ such that @a*x + b*y = 'gcd' a b@
-- (Bézout's identity).
--
-- In particular, if @a@ and @b@ are coprime (i.e. @'gcd' a b == 1@),
--
-- * @y@ is the multiplicative inverse of @b@ modulo @a@.
--
-- * @x@ is the multiplicative inverse of @a@ modulo @b@.
--
-- * @j*a*x + i*b*y@ is equivalent to @i@ modulo @a@ and to @j@ modulo @b@
--   (Chinese Remaider Theorem).
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

-- | For an odd prime @p@, @'legendre' a p@ is @0@ if @a@ is a multiple
-- of @p@, @1@ if @a = x*x@ modulo @p@ for some @x@, and @-1@ otherwise.
legendre :: Int -> Int -> Int
legendre a p = (power_mod (p `div` 2) + 1) `mod` p - 1
  where
    base = a `mod` p
    power_mod n
      | n == 0 = 1
      | odd n = (xx * base) `mod` p
      | otherwise = xx
      where
        x = power_mod (n `div` 2)
        xx = (x * x) `mod` p

-- | @'sqrtMod' a n@ is the list of numbers @x@ such that @a = x*x@ modulo @n@.
sqrtMod :: Int -> Int -> [Int]
sqrtMod a n
  | n <= 0 = error "Non-positive modulus"
  | n == 1 = [0]
  | otherwise = sort (fst (foldr1 combine parts))
  where
    combine (ras, na) (rbs, nb) =
        ([(rb*na*x + ra*nb*y) `mod` nab | ra <- ras, rb <- rbs], nab)
      where
        nab = na*nb
        (x, y) = bezout na nb
    parts = [(roots (a `mod` pk) pk, pk) | pk <- components]
    roots x pk = [y | y <- [0..pk-1], y*y `mod` pk == x]
    components = [p^k | (p, k) <- primeFactors n]
    factors = primeFactors n
