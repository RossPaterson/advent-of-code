module Main where

import Utilities
import Data.List
import Data.Maybe
import Data.Hash.MD5 -- from MissingH package

input_salt :: String
input_salt = "ahsbgdzn"

hash1 :: String -> String
hash1 s = md5s (Str s)

hashes :: String -> (String -> String) -> [(Int, String)]
hashes salt hash = [(n, hash (salt ++ show n)) | n <- [0..]]

-- elements that are repeated at least n times
repetitions :: Eq a => Int -> [a] -> [a]
repetitions n xs = [head g | g <- group xs, length g >= n]

-- first element (if any) that occurs 3 times consecutively in the list
firstTriplet :: Eq a => [a] -> Maybe a
firstTriplet = listToMaybe . repetitions 3

keys :: [(Int, String)] -> [(Int, String)]
keys hs = [(n, s) |
    (n, s):rest <- tails hs,
    c <- maybeToList (firstTriplet s),
    any ((replicate 5 c `isInfixOf`) . snd) (take 1000 rest)]

solve1 :: String -> Int
solve1 salt = fst (keys (hashes salt hash1)!!63)

tests1 :: [(String, Int)]
tests1 = [("abc", 22728)]

-- Part Two

hash2  :: String -> String
hash2 = times 2017 hash1

solve2 :: String -> Int
solve2 salt = fst (keys (hashes salt hash2)!!63)

tests2 :: [(String, Int)]
tests2 = [("abc", 22551)]

main :: IO ()
main = do
    putStr (unlines (failures "solve1" solve1 tests1))
    print (solve1 input_salt)
    -- These are just too slow
    -- putStr (unlines (failures "solve2" solve2 tests2))
    -- print (solve2 input_salt)
