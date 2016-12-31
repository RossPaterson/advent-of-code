module Main where

import Data.List

look_and_say :: String -> String
look_and_say s = concat [show (length g) ++ [head g] | g <- group s]

test = take 6 (iterate look_and_say "1")

input = "1321131112"

solve1 :: String -> Int
solve1 s = length (iterate look_and_say s!!40)

-- Part Two --

solve2 :: String -> Int
solve2 s = length (iterate look_and_say s!!50)

main :: IO ()
main = do
    print (solve1 input)
    print (solve2 input)
