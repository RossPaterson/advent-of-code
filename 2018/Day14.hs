module Main where

import Utilities
import Data.Char
import Data.List
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq

-- Input processing

type Input = String

parse :: String -> Input
parse = head . lines

-- Part One

solve1 :: Input -> String
solve1 = after 10 . read

-- substring of scores of length n starting at position pos
after :: Int -> Int -> String
after n pos = concatMap show $ take n $ drop pos $ allScores initScores

-- initial scores on the board
initScores :: [Int]
initScores = [3, 7]

data State = State (Seq Int) [Int]
  deriving Show

-- infinite list of scores generated from an initial list
allScores :: [Int] -> [Int]
allScores vs = vs ++ concat (unfoldr (Just . grow) (initState vs))

-- start with one worker per initial score
initState :: [Int] -> State
initState vs = State (Seq.fromList vs) [0..length vs-1]

-- add new scores, move the workers' positions, and return the added scores
-- together with the new state
grow :: State -> ([Int], State)
grow (State rs ps) = (added, State rs' ps')
  where
    added = digits (sum [Seq.index rs p | p <- ps])
    rs' = foldl (|>) rs added
    ps' = map moveOne ps
    moveOne p = (p + 1 + Seq.index rs' p) `mod` Seq.length rs'

-- the decimal digits of a number
digits :: Int -> [Int]
digits = map digitToInt . show

tests1 :: [(String, String)]
tests1 = [
    ("9", "5158916779"), ("5", "0124515891"),
    ("18", "9251071085"), ("2018", "5941429882")]

-- Part Two

solve2 :: Input -> Int
solve2 s =
    head $ subsequencePositions (map digitToInt s) (allScores initScores)

-- positions in ys at which xs occurs as a subsequence
subsequencePositions :: Eq a => [a] -> [a] -> [Int]
subsequencePositions xs ys =
    [pos | (pos, tys) <- zip [0..] (tails ys), xs `isPrefixOf` tys]

tests2 :: [(String, Int)]
tests2 = [("51589", 9), ("01245", 5), ("92510", 18), ("59414", 2018)]

main :: IO ()
main = do
    s <- readFile "input/14.txt"
    let input = parse s
    putStr (unlines (failures "solve1" solve1 tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" solve2 tests2))
    print (solve2 input)
