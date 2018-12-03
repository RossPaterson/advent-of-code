module Main where

import Data.Char
import Data.List
import Data.Maybe

incr :: String -> String
incr = reverse . incr' . reverse
  where
    incr' [] = []
    incr' ('z':cs) = 'a':incr' cs
    incr' (c:cs) = chr (ord c + 1) : cs

requirements :: String -> Bool
requirements s =
    any straight (tails s) &&
    and [not (c `elem` s) | c <- "iol"] &&
    or [c1 /= c2 | t <- tails s, c1 <- maybeToList (pair t),
        u <- tails (drop 2 t), c2 <- maybeToList (pair u)]

pair :: String -> Maybe Char
pair (c1:c2:_)
  | c1 == c2 = Just c1
pair _ = Nothing

straight :: String -> Bool
straight (c1:c2:c3:_) = ord c2 == ord c1 + 1 && ord c3 == ord c2 + 1
straight _ = False

input = "cqjxjnds"

solve1 :: String -> String
solve1 = head . filter requirements . iterate incr

-- Part Two --

solve2 :: String -> String
solve2 = head . tail . filter requirements . iterate incr

main :: IO ()
main = do
    putStrLn (solve1 input)
    putStrLn (solve2 input)
