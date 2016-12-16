module Day14 where

import Utilities
import Data.List
import Data.Maybe
import Data.Hash.MD5 -- from MissingH package

salt :: String
salt = "ahsbgdzn"

hash1 :: String -> String
hash1 s = md5s (Str s)

hashes :: String -> (String -> String) -> [(Int, String)]
hashes salt hash = [(n, hash (salt ++ show n)) | n <- [0..]]

firstTriplet :: String -> Maybe Char
firstTriplet s = listToMaybe [c1 | c1:c2:c3:_ <- tails s, c1 == c2, c2 == c3]

keys :: String -> (String -> String) -> [(Int, String)]
keys salt hash = [(n, s) |
    (n, s):rest <- tails (hashes salt hash),
    c <- maybeToList (firstTriplet s),
    any ((replicate 5 c `isInfixOf`) . snd) (take 1000 rest)]

test1 = take 64 (keys "abc" hash1)

puzzle1 :: IO ()
puzzle1 = print (fst (keys salt hash1!!63))

hash2  :: String -> String
hash2 = times 2017 hash1

test2 = take 64 (keys "abc" hash2)

puzzle2 :: IO ()
puzzle2 = print (fst (keys salt hash2!!63))
