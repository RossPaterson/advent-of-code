module Day19 where

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

input :: Int
input = 3017957

puzzle1 = print (solve1 input)

puzzle2 = print (solve2 input)
