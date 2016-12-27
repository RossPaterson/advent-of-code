module Main where

import Data.List

solve1 :: [String] -> Int
solve1 = length . filter nice

nice :: String -> Bool
nice s =
    length [c | c <- s, c `elem` "aeiou"] >= 3 &&
    or (zipWith (==) s (tail s)) &&
    not (or [bad `isInfixOf` s | bad <- ["ab", "cd", "pq", "xy"]])

tests = [
    "ugknbfddgicrmopn",
    "aaa",
    "jchzalrnumimnmhp",
    "haegwjzuvuyypxyu",
    "dvszwmarrgswjxmb"]

-- Part Two --

solve2 :: [String] -> Int
solve2 = length . filter nice2

nice2 :: String -> Bool
nice2 s =
    or [take 2 t `isInfixOf` drop 2 t | t <- tails s, length t >= 2] &&
    not (null [c1 | c1:_:c3:_ <- tails s, c1 == c3])

tests2 = [
    "qjhvhtzxzqqjkmpb",
    "xxyxx",
    "uurcxstgmygtbstg",
    "ieodomkazucvgmuy"]

main :: IO ()
main = do
    s <- readFile "input05.txt"
    let ls = lines s
    print (solve1 ls)
    print (solve2 ls)
