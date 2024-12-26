module Main where

import Utilities

-- Input processing

type Input = (Int, Int)

parse :: String -> Input
parse s = case readNumbers  s of
    [a, b] -> (a, b)
    _ -> error "bad input"

-- Part One

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

solve1 :: Input -> Int
solve1 (a, b) = judge 40000000 $ zip (gen1 a) (gen2 b)

-- Part Two

multiples :: Int -> [Int] -> [Int]
multiples k = filter ((== 0) . (`mod` k))

solve2 :: Input -> Int
solve2 (a, b) =
    judge 5000000 $ zip (multiples 4 (gen1 a)) (multiples 8 (gen2 b))

main :: IO ()
main = do
    s <- readFile "input/15.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
