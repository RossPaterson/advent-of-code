module Main where

import Graph
import Parser
import Utilities
import Control.Applicative
import Data.List
import Data.Function
import Data.Tuple
import Data.Map ((!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = FiniteGraph Node
type Node = String

parse :: String -> Input
parse = relation . bidirectional . map (runParser edge) . lines
  where
    edge = (,) <$> node <* char '-' <*> node
    node = some letter

bidirectional :: [(a, a)] -> [(a, a)]
bidirectional xys = xys ++ map swap xys

-- Part One

solve1 :: Input -> Int
solve1 g = Set.size $ Set.fromList triangles
  where
    -- sort the nodes to normalize the triangles
    triangles = [sort [n1, n2, n3] |
        n1 <- tnodes, n2 <- Set.elems (g!n1), n3 <- Set.elems (g!n1),
        n3 `Set.member` (g!n2)]
    tnodes = filter candidate $ Set.elems nodes
    nodes = Map.keysSet g `Set.union` Set.unions g

candidate :: Node -> Bool
candidate n = head n == 't'

testInput :: String
testInput = "\
    \kh-tc\n\
    \qp-kh\n\
    \de-cg\n\
    \ka-co\n\
    \yn-aq\n\
    \qp-ub\n\
    \cg-tb\n\
    \vc-aq\n\
    \tb-ka\n\
    \wh-tc\n\
    \yn-cg\n\
    \kh-ub\n\
    \ta-co\n\
    \de-co\n\
    \tc-td\n\
    \tb-wq\n\
    \wh-td\n\
    \ta-ka\n\
    \td-qp\n\
    \aq-cg\n\
    \wq-ub\n\
    \ub-vc\n\
    \de-ta\n\
    \wq-aq\n\
    \wq-vc\n\
    \wh-yn\n\
    \ka-de\n\
    \kh-ta\n\
    \co-tc\n\
    \wh-qp\n\
    \tb-vc\n\
    \td-yn\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 7)]

-- Part Two

solve2 :: Input -> String
solve2 = password . maximumBy (compare `on` Set.size) . maximalCliques

password :: Set Node -> String
password = intercalate "," . Set.elems

-- Maximal cliques of the graph
-- algorithm of Tsukiyama et al (1977)
maximalCliques :: Ord a => FiniteGraph a -> [Set a]
maximalCliques g = case Map.lookupMin g of
    Nothing -> [Set.empty]
    Just (n, neighbours) ->
        let subcliques = maximalCliques (remove n g) in
        -- maximal cliques that contain n
        (map (Set.insert n) $ maximalSets $
            map (Set.intersection neighbours) subcliques) ++
        -- maximal cliques that don't contain n
        filter (not . (`Set.isSubsetOf` neighbours)) subcliques

-- remove edges connected to the node
remove :: Ord a => a -> FiniteGraph a -> FiniteGraph a
remove n = Map.delete n . Map.map (Set.delete n)

-- select maximal sets from the list
maximalSets :: Ord a => [Set a] -> [Set a]
maximalSets = foldr add_new [] . sortOn Set.size
  where
    add_new s ss
      | any (s `Set.isSubsetOf`) ss = ss
      | otherwise = s : ss

tests2 :: [(String, String)]
tests2 = [(testInput, "co,de,ka,ta")]

main :: IO ()
main = do
    s <- readFile "input/23.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    putStrLn (solve2 input)
