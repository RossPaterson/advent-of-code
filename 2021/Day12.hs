module Main where

import Utilities
import Parser
import Control.Applicative
import Data.Char
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

data Node n = Start | Middle n | End
    deriving (Eq, Ord, Show)
data Cave = Big String | Small String
    deriving (Eq, Ord, Show)
type BigNode = Node Cave

data Edge = Edge BigNode BigNode
    deriving Show
type Input = [Edge]

parse :: String -> Input
parse = map (runParser edge) . lines
  where
    edge = Edge <$> node <* char '-' <*> node
    node = nodeLabel <$> some letter
    nodeLabel s
      | s == "start" = Start
      | s == "end" = End
      | isUpper (head s) = Middle (Big s)
      | otherwise = Middle (Small s)

-- Part One

type Graph = Map BigNode (Set BigNode)

graph :: [Edge] -> Graph
graph es =
    Map.fromListWith Set.union
        [(s, Set.singleton t) |
            Edge a b <- es, (s, t) <- [(a, b), (b, a)], s /= End, t /= Start]

-- graph of small nodes (plus Start and End) with number of paths between
-- each pair either directly or passing through a big node
type SmallGraph = Map SmallNode (Map SmallNode Int)
type SmallNode = Node String

-- attempt to coerce a big node into a small one
smallNode :: Alternative f => BigNode -> f SmallNode
smallNode Start = pure Start
smallNode (Middle (Small n)) = pure (Middle n)
smallNode (Middle (Big _)) = empty
smallNode End = pure End

-- number of paths between each pair of small caves, either direct or
-- going through a big cave (no two big caves are directly connected)
smallGraph :: Graph -> SmallGraph
smallGraph g =
    Map.fromListWith (Map.unionWith (+))
        [(s, Map.singleton t 1) |
            (big_s, bns) <- Map.assocs g,
            s <- smallNode big_s,
            bn <- Set.elems bns,
            big_t <- neighbours g bn,
            t <- smallNode big_t]

-- neighbours of a big cave, and otherwise the cave itself
neighbours :: Graph -> BigNode -> [BigNode]
neighbours g n@(Middle (Big _)) = maybe [] Set.elems (Map.lookup n g)
neighbours _ n = [n]

-- number of paths to End not visiting any small node twice
pathCount :: Set SmallNode -> SmallNode -> SmallGraph -> Int
pathCount _ End _ = 1
pathCount visited n g
  | Set.member n visited = 0
  | otherwise =
    sum [c*pathCount (Set.insert n visited) n' g | (n', c) <- Map.assocs (g!n)]

solve1 :: Input -> Int
solve1 = pathCount Set.empty Start . smallGraph . graph

testInput1 :: String
testInput1 = "\
    \start-A\n\
    \start-b\n\
    \A-c\n\
    \A-b\n\
    \b-d\n\
    \A-end\n\
    \b-end\n\
    \"

testInput2 :: String
testInput2 = "\
    \dc-end\n\
    \HN-start\n\
    \start-kj\n\
    \dc-start\n\
    \dc-HN\n\
    \LN-dc\n\
    \HN-end\n\
    \kj-sa\n\
    \kj-HN\n\
    \kj-dc\n\
    \"

testInput3 :: String
testInput3 = "\
    \fs-end\n\
    \he-DX\n\
    \fs-he\n\
    \start-DX\n\
    \pj-DX\n\
    \end-zg\n\
    \zg-sl\n\
    \zg-pj\n\
    \pj-he\n\
    \RW-he\n\
    \fs-DX\n\
    \pj-RW\n\
    \zg-RW\n\
    \start-pj\n\
    \he-WI\n\
    \zg-he\n\
    \pj-fs\n\
    \start-RW\n\
    \"

tests1 :: [(String, Int)]
tests1 = [
    (testInput1, 10),
    (testInput2, 19),
    (testInput3, 226)]

-- Part Two

-- small nodes visited and whether we've visited any of them twice
type History = (Set SmallNode, Bool)

initHistory :: History
initHistory = (Set.empty, False)

visit :: SmallNode -> History -> Maybe History
visit n (visited, repeated)
  | Set.member n visited =
    if repeated then Nothing else Just (visited, True)
  | otherwise = Just (Set.insert n visited, repeated)

-- number of paths to End not visiting more than one small node twice
pathCount2 :: History -> SmallNode -> SmallGraph -> Int
pathCount2 _ End _ = 1
pathCount2 hist n g = case visit n hist of
    Nothing -> 0
    Just hist' -> sum [c*pathCount2 hist' n' g | (n', c) <- Map.assocs (g!n)]

solve2 :: Input -> Int
solve2 = pathCount2 initHistory Start . smallGraph . graph

tests2 :: [(String, Int)]
tests2 = [
    (testInput1, 36),
    (testInput2, 103),
    (testInput3, 3509)]

main :: IO ()
main = do
    s <- readFile "input/12.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
