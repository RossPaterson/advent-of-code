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
    totient,
    -- * Extended GCD
    bezout, denominators,
    -- * Modular powers
    modularPower,
    modularRoots,
    modularLogarithm,
    universalExponent,
    isPrimitiveRootOf,
    ) where

import Data.List
import Data.Maybe

-- | The infinite list of prime numbers
-- (OEIS Sequence <http://oeis.org/A000040 A000040>)
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

-- A multiplicative function, specified by its action on @p^k@
multiplicative :: Num a => (Int -> Int -> a) -> Int -> a
multiplicative f n = product [f p k | (p, k) <- primeFactors n]

-- | The number of divisors of @n@
-- (OEIS Sequence <http://oeis.org/A000005 A000005>)
numberOfDivisors :: Int -> Int
numberOfDivisors = multiplicative (const (+1))

-- | The sum of all the divisors of @n@
-- (OEIS Sequence <http://oeis.org/A000203 A000203>)
sumOfDivisors :: Int -> Integer
sumOfDivisors = sumOfDivisorPowers 1

-- | @'sumOfDivisorPowers' m n@ is the sum of @m@th powers all the
-- divisors of @n@.
sumOfDivisorPowers :: Int -> Int -> Integer
sumOfDivisorPowers m
  | m <= 0 = toInteger . numberOfDivisors
  | otherwise = multiplicative $ \ p k -> sumPowers (toInteger p^m) k

-- @'sumPowers' x k = x^k + ... + x + 1@
sumPowers :: Integer -> Int -> Integer
sumPowers x k = fromInteger ((x^(k+1) - 1) `div` (x-1))

-- | @'totient' n@ is the number of numbers between @1@ and @n@ that
-- are coprime with @n@ (Euler's totient function,
-- OEIS Sequence <http://oeis.org/A000010 A000010>).
totient :: Int -> Int
totient = multiplicative $ \ p k -> p^(k-1)*(p - 1)

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
--   (Chinese Remainder Theorem).
bezout :: Integral a => a -> a -> (a, a)
bezout a b = (approx (signum a) 0 qs, approx 0 (signum b) qs)
  where
    qs = denominators (abs a) (abs b)

    approx x _ [] = x
    approx x y (c:cs) = approx y (x - c*y) cs

-- | @'denominators' a b@ is the list of partial denominators of a
-- regular continued fraction representation of @a/b@.
denominators :: Integral a => a -> a -> [a]
denominators a b
  | b == 0 = []
  | otherwise = q:denominators b r
  where
    (q, r) = divMod a b

-- | @'modularPower' n a k = a^k `mod` n@
modularPower :: Integral a => a -> a -> Int -> a
modularPower n a = power_mod
  where
    base = a `mod` n
    power_mod k
      | k == 0 = 1
      | odd k = (base * xx) `mod` n
      | otherwise = xx
      where
        x = power_mod (k `div` 2)
        xx = (x * x) `mod` n

-- | @'modularRoots' n b k@ is the list of @a@ such that
-- @a^k@ &#x2261; @b@ (mod @n@).
modularRoots :: Integral a => a -> a -> Int -> [a]
modularRoots n b i
  | n <= 0 = error "Non-positive modulus"
  | n == 1 = [0]
  | i == 0 = [b `mod` n]
  | otherwise = sort (fst (foldr1 combine components))
  where
    combine (ras, na) (rbs, nb) =
        ([(rb*na*x + ra*nb*y) `mod` nab | ra <- ras, rb <- rbs], nab)
      where
        nab = na*nb
        (x, y) = bezout na nb
    components = [(naiveModularRoots pk b i, pk) |
        (p, k) <- primeFactors (fromIntegral n), let pk = fromIntegral (p^k)]

naiveModularRoots :: Integral a => a -> a -> Int -> [a]
naiveModularRoots n b k = [a | a <- [0..n-1], modularPower n a k == b_mod_n]
  where
    b_mod_n = b `mod` n

-- | @'modularLogarithm' n a b@ is the least @k@ such that
-- @a^k@ &#x2261; @b@ (mod @n@).
modularLogarithm :: Integral a => a -> a -> a -> Maybe Int
modularLogarithm n a b =
    listToMaybe [k | k <- [1..universalExponent (fromIntegral n) - 1],
        modularPower n a k == b_mod_n]
  where
    b_mod_n = b `mod` n

-- | @'universalExponent' n@ is the smallest @k@ such that @a^k@ &#x2261;
-- @1@ (mod @n@) for each @a@ between @1@ and @n@ that is coprime with @n@
-- (Carmichael function, OEIS Sequence <http://oeis.org/A002322 A002322>).
universalExponent :: Int -> Int
universalExponent = foldr lcm 1 . map carmichael . primeFactors
  where
    carmichael (p, k)
      | p == 2 && k >= 3 = euler `div` 2
      | otherwise = euler
      where
        euler = p^(k-1)*(p-1)

-- | @'isPrimitiveRootOf' a n@ is 'True' if all the numbers less than @n@
-- coprime with @n@ are powers of @a@.
isPrimitiveRootOf :: Int -> Int -> Bool
isPrimitiveRootOf a n =
    gcd a n == 1 &&
    and [modularPower n a k /= 1 |
        k <- [phi `div` p | (p, _) <- primeFactors phi]]
  where
    phi = totient n
