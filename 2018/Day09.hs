module Main where

import Parser
import Utilities
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq, ViewL(..), ViewR(..), (<|), (|>), (><))
import qualified Data.Sequence as Seq

-- Input processing

type Input = (Int, Int)

parse :: String -> Input
parse = runParser parameters
  where
    parameters = (,) <$> nat <* string " players; last marble is worth " <*>
        nat <* string " points\n"

-- Part One

solve1 :: Input -> Integer
solve1 = maximum . map snd . Map.toList . scores . uncurry marbles

-- State of the game
data State = State {
    circle :: Seq Int, -- circle of marbles unrolled with current at left end
    scores :: Map Int Integer -- score for each player
    }
  deriving Show

-- final state after adding nm marbles to np players
marbles :: Int -> Int -> State
marbles np nm = foldl add (initState np) [1..nm]

-- a circle containing just marble 0, and all scores zero
initState :: Int -> State
initState np = State {
    circle = Seq.singleton 0,
    scores = Map.fromList [(p, 0) | p <- [1..np]]
    }

-- add a marble to the game, with peculiar rule for multiples of 23
add :: State -> Int -> State
add (State c ss) n
  | n `mod` 23 /= 0 = State (n <| rotl (rotl c)) ss
  | otherwise = State c' (Map.adjust (+ toInteger (n+m)) player ss)
  where
    player = (n - 1) `mod` Map.size ss + 1
    -- extract the marble 7 places counter-clockwise from current
    (l, r) = Seq.splitAt (Seq.length c - 6) c
    (c', m) = case Seq.viewr l of
        l' :> x -> (r >< l', x)
        EmptyR -> error "can't happen"

-- rotate a sequence one position to the left
rotl :: Seq a -> Seq a
rotl c = case Seq.viewl c of
    EmptyL -> Seq.empty
    x :< xs -> xs |> x

tests1 :: [(String, Integer)]
tests1 = [
    ("9 players; last marble is worth 25 points\n",  32),
    ("10 players; last marble is worth 1618 points\n", 8317),
    ("13 players; last marble is worth 7999 points\n", 146373),
    ("17 players; last marble is worth 1104 points\n", 2764),
    ("21 players; last marble is worth 6111 points\n", 54718),
    ("30 players; last marble is worth 5807 points\n", 37305)]

-- Part Two

solve2 :: Input -> Integer
solve2 (np, nm) = solve1 (np, nm*100)

main :: IO ()
main = do
    s <- readFile "input/09.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    print (solve2 input)
