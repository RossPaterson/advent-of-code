module Main where

import MD5
import Utilities
import Data.List

hash :: String -> Int -> String
hash s n = md5s (s ++ show n)

solve1 :: String -> Int
solve1 s = head [n | n <- [0..], let h = hash s n, "00000" `isPrefixOf` h]

tests1 :: [(String, Int)]
tests1 = [
    ("abcdef", 609043),
    ("pqrstuv", 1048970)]

-- Part Two

solve2 :: String -> Int
solve2 s = head [n | n <- [0..], let h = hash s n, "000000" `isPrefixOf` h]

main :: IO ()
main = do
    s <- readFile "input/04.txt"
    let input = head (lines s)
    putStr (unlines (failures "solve1" solve1 tests1))
    print (solve1 input)
    print (solve2 input)
