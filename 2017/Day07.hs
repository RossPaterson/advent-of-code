module Main where

import Parser
import Utilities
import Control.Applicative
import Data.Char
import Data.List
import Data.Ord
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type Weight = Int
type Name = String
type Input = Map Name (Weight, [Name])

parse :: String -> Input
parse = Map.fromList . map (runParser report) . lines
  where
    report = (,) <$> name <*> ((,) <$ string " (" <*> nat <* string ")" <*> top)
    top = pure [] <|> string " -> " *> sepBy1 name (string ", ")
    name = some (satisfy isLower)

nonRoot :: Input -> Set Name
nonRoot = foldMap (Set.fromList . snd)

root :: Input -> Name
root m = case Set.toList (Set.difference (Map.keysSet m) (nonRoot m)) of
    [] -> error "no root"
    [n] -> n
    _ -> error "multiple roots"

solve1 :: Input -> Name
solve1 = root

testInput :: String
testInput =
    "pbga (66)\n\
    \xhth (57)\n\
    \ebii (61)\n\
    \havc (66)\n\
    \ktlj (57)\n\
    \fwft (72) -> ktlj, cntj, xhth\n\
    \qoyq (66)\n\
    \padx (45) -> pbga, havc, qoyq\n\
    \tknk (41) -> ugml, padx, fwft\n\
    \jptl (61)\n\
    \ugml (68) -> gyxo, ebii, jptl\n\
    \gyxo (61)\n\
    \cntj (57)\n"

tests1 :: [(String, Name)]
tests1 = [(testInput, "tknk")]

-- Part Two

data Tower = Program Name Weight [Tower]
  deriving Show

mkTower :: Input -> Tower
mkTower m = mk (root m)
  where
    mk label = case Map.lookup label m of
        Nothing -> error label
        Just (w, labels) -> Program label w (map mk labels)

weight :: Tower -> Weight
weight (Program _ w ts) = w + sum (map weight ts)

allsame :: Eq a => [a] -> Bool
allsame [] = True
allsame (x:xs) = all (== x) xs

balanced :: Tower -> Bool
balanced (Program _ _ ts) = allsame (map weight ts)

majority :: Ord a => [a] -> a
majority xs = head (maximumBy (comparing length) (group (sort xs)))

rebalance :: Tower -> Int
rebalance (Program _ _ ts) =
    case filter (not . balanced) ts of
        [] -> balanceChildren ts
        [t] -> rebalance t
        _ -> error "more than one unbalanced subtree"

-- adjust one of several balanced subtrees to make their weights equal
balanceChildren :: [Tower] -> Int
balanceChildren ts =
    head [w + majorityWeight - wt |
        t@(Program _ w _) <- ts, let wt = weight t, wt /= majorityWeight]
  where
    majorityWeight = majority (map weight ts)

solve2 :: Input -> Int
solve2 = rebalance . mkTower

tests2 :: [(String, Int)]
tests2 = [(testInput, 60)]

main :: IO ()
main = do
    s <- readFile "input/07.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    putStrLn (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
