module Main where

import Utilities
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Hash.MD5 -- from MissingH package

hash :: String -> Int -> String
hash s n = md5s (Str (s ++ show n))

matches :: String -> [String]
matches s = [drop 5 h | n <- [0..], let h = hash s n, take 5 h == "00000"]

solve1 :: String -> String
solve1 s = take 8 (map head (matches s))

tests1 :: [(String, String)]
tests1 = [("abc", "18f47a30")]

-- Part Two

solve2 :: String -> String
solve2 s =
    Map.elems $ head $
        dropWhile ((< 8) . Map.size) $ scanl add Map.empty $ matches s

type State = Map Int Char

add :: State -> String -> State
add m (pos:c:_)
  | i < 8 && Map.notMember i m = Map.insert i c m
  | otherwise = m
  where
    i = ord pos - ord '0'
add _ _ = error "string is too short"

tests2 :: [(String, String)]
tests2 = [("abc", "05ace8e3")]

input :: String
input = "ojvtpuvg"

main :: IO ()
main = do
    putStr (unlines (failures "solve1" solve1 tests1))
    putStrLn (solve1 input)
    putStr (unlines (failures "solve2" solve2 tests2))
    putStrLn (solve2 input)
