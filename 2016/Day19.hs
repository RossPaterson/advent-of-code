module Main where

import Utilities
import Data.Sequence (Seq, ViewL(..), ViewR(..), (|>), (><))
import qualified Data.Sequence as Seq

-- original positions only -- number of presents is unused
type State = Seq Int

initState :: Int -> State
initState n = Seq.fromList $ [1..n]

play :: State -> Int
play elves = case Seq.viewl elves of
    EmptyL -> error "empty list"
    e :< elves' -> case Seq.viewl elves' of
        EmptyL -> e
        _ :< elves'' -> play (elves'' |> e)

solve1 :: Int -> Int
solve1 = play . initState

tests1 :: [(Int, Int)]
tests1 = [(5, 3)]

-- Part Two --

play2 :: State -> Int
play2 elves = case Seq.viewl elves of
    EmptyL -> error "empty list"
    e :< elves' -> case Seq.viewr left of
        EmptyR -> e
        left' :> _ -> play2 ((left' >< right) |> e)
      where
        n = Seq.length elves'
        (left, right) = Seq.splitAt ((n+1) `div` 2) elves'

solve2 :: Int -> Int
solve2 = play2 . initState

tests2 :: [(Int, Int)]
tests2 = [(5, 2)]

input :: Int
input = 3017957

main :: IO ()
main = do
    putStr (unlines (failures "solve1" solve1 tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" solve2 tests2))
    print (solve2 input)
