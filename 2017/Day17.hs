module Main where

import Utilities
import Data.Foldable
import Data.Sequence (Seq, (|>), (><))
import qualified Data.Sequence as Seq

type Input = Int

-- last inserted value at right end
type State = Seq Int

initState :: State
initState = Seq.singleton 0

step :: Input -> State -> Int -> State
step incr s n = case Seq.splitAt (incr `mod` Seq.length s) s of
    (front, back) -> (back >< front) |> n

valAfter :: Int -> Input -> Int
valAfter n incr = head $ toList $ last $ scanl (step incr) initState [1..n]

solve1 :: Input -> Int
solve1 = valAfter 2017

tests1 :: [(Input, Int)]
tests1 = [(3, 638)]

-- Part Two

data State2 = State2 { lastInsert :: Int, position :: Int }
    deriving Show

initState2 :: State2
initState2 = State2 0 0

step2 :: Input -> State2 -> State2
step2 incr s =
    State2 { lastInsert = n, position = (position s + incr) `mod` n + 1 }
  where
    n = lastInsert s + 1

valFollowing0 :: Int -> Input -> Int
valFollowing0 n incr =
   last [lastInsert s |
       s <- take (n+1) (iterate (step2 incr) initState2), position s == 1]

solve2 :: Input -> Int
solve2 = valFollowing0 50000000

tests2 :: [((Int, Input), Int)]
tests2 = [((9, 3), 9)]

input :: Input
input = 304

main :: IO ()
main = do
    putStr (unlines (failures "solve1" solve1 tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (uncurry valFollowing0) tests2))
    print (solve2 input)
