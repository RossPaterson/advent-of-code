module Main where

import Parser
import Utilities
import Control.Applicative
import Data.List
import Data.Map ((!), Map)
import qualified Data.Map as Map

-- Input processing

type Input = (Path, Network)

type Path = [Dir]
data Dir = L | R
    deriving (Bounded, Enum, Show)

type Network = Map Node (Node, Node)
type Node = String

parse :: String -> Input
parse s = case map lines (paragraphs s) of
    [[path_s], rules_s] ->
        (runParser path path_s, Map.fromList (map (runParser rule) rules_s))
    _ -> error "bad input"
  where
    path = some enumValue
    rule = (,) <$> node <* string " = " <*> rhs
    rhs = (,) <$ char '(' <*> node <* string ", " <*> node <* char ')'
    node = some (digit <|> letter)

-- Part One

-- move from a node to the left or right nodes in the network
step :: Network -> Node -> Dir -> Node
step net node L = fst (net!node)
step net node R = snd (net!node)

-- the list of nodes reached by folowing a path from a start node
follow :: Network -> Node -> Path -> [Node]
follow net = scanl (step net)

solve1 :: Input -> Int
solve1 (path, net) =
    length $ takeWhile (/= "ZZZ") $ follow net "AAA" $ cycle path

testInput :: String
testInput = "\
    \RL\n\
    \\n\
    \AAA = (BBB, CCC)\n\
    \BBB = (DDD, EEE)\n\
    \CCC = (ZZZ, GGG)\n\
    \DDD = (DDD, DDD)\n\
    \EEE = (EEE, EEE)\n\
    \GGG = (GGG, GGG)\n\
    \ZZZ = (ZZZ, ZZZ)\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 2)]

-- Part Two

-- start nodes of the network
starts :: Network -> [Node]
starts net = filter (isSuffixOf "A") (Map.keys net)

-- is this a finish node?
finish :: Node -> Bool
finish = isSuffixOf "Z"

-- number of steps from a start node to a finish node (which happens to
-- be equal to the number of steps to return to that finish node)
period :: Path -> Network -> Node -> Int
period path net start =
    length $ takeWhile (not . finish) $ follow net start $ cycle path

{-
We are told that there is a one-to-one correspondence between start
and finish nodes.  It also turns out that the supplied inputs have the
properties that

* for each start node s_i, one reaches the corresponding finish node
  f_i in n_i steps, where n_i is a multiple of the path length, and

* from f_i, one reaches f_i again in another n_i steps.

Thus the number of steps until all traversals are at finish nodes is the
least common multiple of the n_i's.  (If the two numbers for each pair of
nodes had not been the same, this would be a Chinese Remainder Problem.)
-}

solve2 :: Input -> Int
solve2 (path, net) =
    foldr1 lcm $ map (period path net) $ starts net

testInput2 :: String
testInput2 = "\
    \LR\n\
    \\n\
    \11A = (11B, XXX)\n\
    \11B = (XXX, 11Z)\n\
    \11Z = (11B, XXX)\n\
    \22A = (22B, XXX)\n\
    \22B = (22C, 22C)\n\
    \22C = (22Z, 22Z)\n\
    \22Z = (22B, 22B)\n\
    \XXX = (XXX, XXX)\n"

tests2 :: [(String, Int)]
tests2 = [(testInput2, 6)]

main :: IO ()
main = do
    s <- readFile "input/08.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
