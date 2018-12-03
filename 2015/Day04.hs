module Main where

import Data.Char
import Data.List
import Data.Map hiding (map, foldl)
import Data.Hash.MD5 -- from MissingH package

hash :: String -> Int -> String
hash s n = md5s (Str (s ++ show n))

solve1 :: String -> Int
solve1 s = head [n | n <- [0..], let h = hash s n, "00000" `isPrefixOf` h]

test = "abcdef"

input = "yzbqklnj"

-- Part Two

solve2 :: String -> Int
solve2 s = head [n | n <- [0..], let h = hash s n, "000000" `isPrefixOf` h]

main :: IO ()
main = do
    print (solve1 input)
    print (solve2 input)
