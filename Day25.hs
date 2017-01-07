module Main where

-- position in an infinite sequence of (row, column) coordinates in the
-- diagonalization
index :: Int -> Int -> Int
index r c = before + c
  where
    diagonal = r + c - 1
    before = diagonal*(diagonal-1) `div` 2

-- strict version of iterate
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : (iterate' f $! f x)

numbers :: [Integer]
numbers = iterate' step 20151125
  where
    step n = n * 252533 `mod` 33554393::Integer

getNumber :: Int -> Int -> Integer
getNumber r c = numbers !! (index r c - 1)

main :: IO ()
main = do
    print (getNumber 2981 3075)
