module Main where

import Utilities
import Parser
import Control.Applicative
import Data.Functor
import Data.Maybe

-- Input processing

data Tree = Leaf Int | Pair Tree Tree
    deriving (Eq, Show)

showTree :: Tree -> String
showTree t = showsTree t ""
  where
    showsTree (Leaf n) = shows n
    showsTree (Pair l r) =
        showChar '[' . showsTree l . showChar ',' . showsTree r . showChar ']'

type Input = [Tree]

parse :: String -> Input
parse = map (runParser tree) . lines
  where
    tree =
        Leaf <$> nat <|>
        Pair <$ char '[' <*> tree <* char ',' <*> tree <* char ']'

-- Part One

-- explode action

explode :: Tree -> Maybe Tree
explode = fmap middle . explodeDepth 0
  where
    middle (_, t, _) = t

-- The outer Maybe indicates whether a change occurred to the tree.
-- In the Just case, the inner triple comprises:
-- * a number to be added to the left (if not already added)
-- * the modified tree
-- * a number to be added to the right (if not already added)
explodeDepth :: Int -> Tree -> Maybe (Maybe Int, Tree, Maybe Int)
explodeDepth _ (Leaf _) = Nothing
explodeDepth n (Pair (Leaf x) (Leaf y))
  | n >= 4 = Just (Just x, Leaf 0, Just y)
explodeDepth n (Pair l r) =
    (explodeDepth (n+1) l <&> \ (mb_x, l', mb_y) ->
        (mb_x, Pair l' (fromMaybe id (addFirst <$> mb_y) r), Nothing)) <|>
    (explodeDepth (n+1) r <&> \ (mb_x, r', mb_y) ->
        (Nothing, Pair (fromMaybe id (addLast <$> mb_x) l) r', mb_y))

-- add x to the leftmost leaf
addFirst :: Int -> Tree -> Tree
addFirst x (Leaf y) = Leaf (x+y)
addFirst x (Pair l r) = Pair (addFirst x l) r

-- add x to the rightmost leaf
addLast :: Int -> Tree -> Tree
addLast x (Leaf y) = Leaf (x+y)
addLast x (Pair l r) = Pair l (addLast x r)

-- split action

-- The Maybe indicates whether a change occurred to the tree.
split :: Tree -> Maybe Tree
split (Leaf n)
  | n >= 10 = Just (Pair (Leaf (n `div` 2)) (Leaf ((n+1) `div` 2)))
  | otherwise = Nothing
split (Pair l r) =
    (split l <&> \ l' -> Pair l' r) <|>
    (split r <&> \ r' -> Pair l r')

-- reduction

action :: Tree -> Maybe Tree
action t = explode t <|> split t

reduce :: Tree -> Tree
reduce = whileJust action

-- "addition" of trees
add :: Tree -> Tree -> Tree
add l r = reduce (Pair l r)

magnitude :: Tree -> Int
magnitude (Leaf n) = n
magnitude (Pair l r) = 3*magnitude l + 2*magnitude r

solve1 :: Input -> Int
solve1 = magnitude . foldl1 add

testInput :: String
testInput = "\
    \[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]\n\
    \[[[5,[2,8]],4],[5,[[9,9],0]]]\n\
    \[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]\n\
    \[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]\n\
    \[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]\n\
    \[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]\n\
    \[[[[5,4],[7,7]],8],[[8,3],8]]\n\
    \[[9,3],[[9,9],[6,[4,9]]]]\n\
    \[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]\n\
    \[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 4140)]

-- Part Two

solve2 :: Input -> Int
solve2 ts = maximum [magnitude (add x y) | x <- ts, y <- ts, x /= y]

tests2 :: [(String, Int)]
tests2 = [(testInput, 3993)]

main :: IO ()
main = do
    s <- readFile "input/18.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
