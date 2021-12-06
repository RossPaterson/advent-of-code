-- | Arbitrary-sized matrices.
module Matrix (Matrix(..), apply) where

-- based on a haskell-cafe posting by Udo Stenzel on 22 Jun 2006.

import Data.List (transpose)

-- | Arbitrary-sized matrices.
newtype Matrix a = Matrix [[a]] -- ^ list of rows
    deriving (Eq, Show)

instance Functor Matrix where
    fmap f (Matrix as) = Matrix (fmap (fmap f) as)

-- | Beware that the identity matrix is infinite.
instance (Num a) => Num (Matrix a) where
    Matrix as + Matrix bs = Matrix (zipWith (zipWith (+)) as bs)
    Matrix as - Matrix bs = Matrix (zipWith (zipWith (-)) as bs)
    negate (Matrix as) = Matrix (map (map negate) as)
    Matrix as * Matrix bs = Matrix [[dot a b | b <- transpose_bs] | a <- as]
      where
        transpose_bs = transpose bs
    fromInteger x = Matrix (iterate (0:) (fromInteger x : repeat 0))
    abs _ = error "abs: undefined for Matrix"
    signum _ = error "signum: undefined for Matrix"

-- | Multiply a matrix by a vector.
apply :: (Num a) => Matrix a -> [a] -> [a]
apply (Matrix as) b = [dot a b | a <- as]

dot :: Num a => [a] -> [a] -> a
dot xs ys = sum (zipWith (*) xs ys)
