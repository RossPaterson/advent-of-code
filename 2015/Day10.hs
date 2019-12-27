module Main where

import Data.List

-- look-and-say sequence (John Conway)
look_and_say :: String -> [String]
look_and_say = iterate count
  where
    count s = concat [show (length g) ++ [head g] | g <- group s]

-- A005150 in OEIS
test :: [String]
test = take 6 (look_and_say "1")

input :: String
input = "1321131112"

solve1 :: String -> Int
solve1 s = length (look_and_say s!!40)

-- Part Two --

solve2 :: String -> Int
solve2 s = length (look_and_say s!!50)

main :: IO ()
main = do
    print (solve1 input)
    print (solve2 input)
