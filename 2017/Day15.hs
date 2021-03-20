module Main where

import Utilities

next :: Int -> Int -> Int
next f v = f*v `mod` 2147483647

gen1, gen2 :: Int -> [Int]
gen1 = iterate (next 16807)
gen2 = iterate (next 48271)

low16 :: Int -> Int
low16 x = x `mod` 65536

match :: (Int, Int) -> Bool
match (x, y) = low16 x == low16 y

judge :: Int -> [(Int, Int)] -> Int
judge n = length . filter match . take n

solve1 :: Int -> Int -> Int
solve1 input1 input2 = judge 40000000 (zip (gen1 input1) (gen2 input2))

-- Part Two

multiples :: Int -> [Int] -> [Int]
multiples k = filter ((== 0) . (`mod` k))

solve2 :: Int -> Int -> Int
solve2 input1 input2 = judge 5000000 $
    zip (multiples 4 (gen1 input1)) (multiples 8 (gen2 input2))

main :: IO ()
main = do
    s <- readFile "input/15.txt"
    let [a, b] = readNumbers s
    print (solve1 a b)
    print (solve2 a b)
