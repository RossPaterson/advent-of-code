module Day05 where

import Data.Char
import Data.Map hiding (map, foldl)
import Data.Hash.MD5 -- from MissingH package

hash :: String -> Int -> String
hash s n = md5s (Str (s ++ show n))

matches :: String -> [String]
matches s = [drop 5 h | n <- [0..], let h = hash s n, take 5 h == "00000"]

decode :: String -> String
decode s = take 8 (map head (matches s))

test = decode "abc"
puzzle1 = decode "ojvtpuvg"

decode2 :: String -> String
decode2 s = elems $ head $ dropWhile ((< 8) . size) $ scanl add empty $ matches s

type State = Map Int Char

add :: State -> String -> State
add m (pos:c:_)
  | i < 8 && notMember i m = insert i c m
  | otherwise = m
  where
    i = ord pos - ord '0'

test2 = decode2 "abc"
puzzle2 = decode2 "ojvtpuvg"
