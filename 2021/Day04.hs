module Main where

import Utilities
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Board = [[Int]]

type Input = ([Int], [Board])

parse :: String -> Input
parse s = case paragraphs s of
    (p:ps) -> (readNumbers p, map (map (map read . words) . lines) ps)
    _ -> error "bad input"

-- Part One

-- rows and columns of a player's board, minus any numbers drawn so far
type Player = [Set Int]

rows_and_cols :: Board -> Player
rows_and_cols rs = map Set.fromList rs ++ map Set.fromList (transpose rs)

-- mark a player's board
mark :: Int -> Player -> Player
mark n = map (Set.delete n)

-- a player has won if any or their rows or columns is finished
won :: Player -> Bool
won = any Set.null

-- mark players' boards with a newly drawn number and pick out any winners
draw :: [Player] -> Int -> ([Player], (Int, [Player]))
draw ps n = (others, (n, winners))
  where
    (winners, others) = partition won (map (mark n) ps)

-- winning boards, with the number on which they won, in draw order
wins :: Input -> [(Int, Player)]
wins (ns, bs) =
    [(n, p) |
        (n, ps) <- snd (mapAccumL draw (map rows_and_cols bs) ns),
        p <- ps]

score :: Int -> Player -> Int
score n b = n * sum (Set.unions b)

solve1 :: Input -> Int
solve1 = uncurry score . head . wins

testInput :: String
testInput = "\
    \7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1\n\
    \\n\
    \22 13 17 11  0\n\
    \ 8  2 23  4 24\n\
    \21  9 14 16  7\n\
    \ 6 10  3 18  5\n\
    \ 1 12 20 15 19\n\
    \\n\
    \ 3 15  0  2 22\n\
    \ 9 18 13 17  5\n\
    \19  8  7 25 23\n\
    \20 11 10 24  4\n\
    \14 21 16 12  6\n\
    \\n\
    \14 21 17 24  4\n\
    \10 16 15  9 19\n\
    \18  8 23 26 20\n\
    \22 11 13  6  5\n\
    \ 2  0 12  3  7\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 4512)]

-- Part Two

solve2 :: Input -> Int
solve2 = uncurry score . last . wins

tests2 :: [(String, Int)]
tests2 = [(testInput, 1924)]

main :: IO ()
main = do
    s <- readFile "input/04.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
