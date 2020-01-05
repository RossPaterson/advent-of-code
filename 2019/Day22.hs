module Main where

import Utilities
import Parser
import Number
import Control.Applicative
import Data.List
import Data.Ord
import Data.Semigroup

-- Input processing

type Input = [Technique]

data Technique = Cut Int | NewStack | Increment Int
    deriving Show

parse :: String -> Input
parse = map (runParser technique) . lines
  where
    technique =
        Cut <$ string "cut " <*> int <|>
        NewStack <$ string "deal into new stack" <|>
        Increment <$ string "deal with increment " <*> nat

-- Part One

-- linear transformation mod n
data Linear = Linear Integer Integer Integer
    deriving Show

apply :: Linear -> Int -> Int
apply (Linear n a b) i = fromInteger ((a * toInteger i + b) `mod` n)

instance Semigroup Linear where
    -- apply (t1 <> t2) = apply t2 (apply t1)
    Linear n1 a1 b1 <> Linear n2 a2 b2
      | n1 == n2 = Linear n1 (a1*a2 `mod` n1) ((a2*b1 + b2) `mod` n1)
      | otherwise = error "combination of different moduli"

-- constructor from Ints
linear :: Int -> Int -> Int -> Linear
linear n a b = Linear (toInteger n) (toInteger a) (toInteger b)

-- linear transformation (modulo n) on indices defined by the technique
transform :: Int -> Technique -> Linear
transform n NewStack = linear n (-1) (-1)
transform n (Cut k) = linear n 1 (-k)
transform n (Increment k) = linear n k 0

transforms :: Int -> [Technique] -> Linear
transforms n = foldl1 (<>) . map (transform n)

solve1 :: Input -> Int
solve1 ts = apply (transforms 10007 ts) 2019

-- apply the techniques to a list of numbers for testing
shuffle :: Int -> [Technique] -> [Int]
shuffle n ts = permute (apply (transforms n ts)) [0..n-1]

-- re-arrange elements of a list given permutation of indices
permute :: (Int -> Int) -> [a] -> [a]
permute f xs =
    map snd (sortBy (comparing fst) [(f n, x) | (n, x) <- zip [0..] xs])

tests1 :: [(String, [Int])]
tests1 = [
    ("deal with increment 7\n\
     \deal into new stack\n\
     \deal into new stack\n",
     [0, 3, 6, 9, 2, 5, 8, 1, 4, 7]),
    ("cut 6\n\
     \deal with increment 7\n\
     \deal into new stack\n",
     [3, 0, 7, 4, 1, 8, 5, 2, 9, 6]),
    ("deal with increment 7\n\
     \deal with increment 9\n\
     \cut -2\n",
     [6, 3, 0, 7, 4, 1, 8, 5, 2, 9]),
    ("deal into new stack\n\
     \cut -2\n\
     \deal with increment 7\n\
     \cut 8\n\
     \cut -4\n\
     \deal with increment 7\n\
     \cut 3\n\
     \deal with increment 9\n\
     \deal with increment 3\n\
     \cut -1\n",
     [9, 2, 5, 8, 1, 4, 7, 0, 3, 6])]

-- Part Two

-- inverse of a linear transformation modulo n
inverse :: Linear -> Linear
inverse (Linear n a b) = Linear n inv_a ((- inv_a*b) `mod` n)
  where
    inv_a = snd (bezout n a) `mod` n

solve2 :: Input -> Int
solve2 ts =
    apply (inverse (stimes 101741582076661 (transforms 119315717514047 ts))) 2020

main :: IO ()
main = do
    s <- readFile "input/22.txt"
    let input = parse s
    putStr (unlines (failures "shuffle" (shuffle 10 . parse) tests1))
    print (solve1 input)
    print (solve2 input)
