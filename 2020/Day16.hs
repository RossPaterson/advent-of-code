module Main where

import Utilities
import Matching
import Parser
import Control.Applicative
import Data.List
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

data Input = Input {
    field_rules :: [Rule],
    your_ticket :: Ticket,
    others :: [Ticket] }
    deriving Show

type Rule = (String, Ranges)

type Ranges = (Range, Range)

data Range = Range Int Int
    deriving Show

type Ticket = [Int]

parse :: String -> Input
parse s = Input {
    field_rules = map (runParser rule) p1,
    your_ticket = runParser ticket (p2!!1),
    others = map (runParser ticket) (tail p3)
    }
  where
    rule = (,) <$> some (satisfy (/= ':')) <* string ": " <*> ranges
    ranges = (,) <$> range <* string " or " <*> range
    range = Range <$> nat <* char '-' <*> nat
    ticket = sepBy1 nat (char ',')
    [p1, p2, p3] = map lines (paragraphs s)

-- Part One

within_range :: Range -> Int -> Bool
within_range (Range lo hi) v = lo <= v && v <= hi

within_ranges :: Ranges -> Int -> Bool
within_ranges (r1, r2) v = within_range r1 v || within_range r2 v

-- Is the number permissible under any of the rules?
possible :: [Rule] -> Int -> Bool
possible rs v = or [within_ranges r v | (_, r) <- rs]

-- numbers on the ticket not permissible under any rule
invalid_nos :: [Rule] -> Ticket -> [Int]
invalid_nos rs = filter (not . possible rs)

solve1 :: Input -> Int
solve1 d = sum $ concat $ map (invalid_nos (field_rules d)) $ others d

testInput :: String
testInput = "\
    \class: 1-3 or 5-7\n\
    \row: 6-11 or 33-44\n\
    \seat: 13-40 or 45-50\n\
    \\n\
    \your ticket:\n\
    \7,1,14\n\
    \\n\
    \nearby tickets:\n\
    \7,3,47\n\
    \40,4,50\n\
    \55,2,20\n\
    \38,6,12\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 71)]

-- Part Two

-- the ticket does not contain any impossible values
valid :: [Rule] -> Ticket -> Bool
valid rs vs = null (invalid_nos rs vs)

-- consistent assignment of field names to field indices
field_coding :: [Rule] -> [Ticket] -> [(String, Int)]
field_coding rs ts =
    Map.assocs $ uniquePerfectMatching $
        Map.fromList $ possible_rule_fields rs ts

-- for each rule name, the set of field indices that hold only valid values
possible_rule_fields :: [Rule] -> [Ticket] -> [(String, Set Int)]
possible_rule_fields rs ts = [(nr, candidates r) | (nr, r) <- rs]
  where
    candidates r =
        Set.fromList [nf | (nf, vs) <- nvs, all (within_ranges r) vs]
    nvs = zip [0..] (transpose ts)

solve2 :: Input -> Int
solve2 (Input rs t ts) =
    product [t!!f | (n, f) <- coding, isPrefixOf "departure" n]
  where
    coding = field_coding rs (t:filter (valid rs) ts)

testInput2 :: String
testInput2 = "\
    \class: 0-1 or 4-19\n\
    \row: 0-5 or 8-19\n\
    \seat: 0-13 or 16-19\n\
    \\n\
    \your ticket:\n\
    \11,12,13\n\
    \\n\
    \nearby tickets:\n\
    \3,9,18\n\
    \15,1,5\n\
    \5,14,9\n"

tests2 :: [(String, [(String, Int)])]
tests2 = [(testInput2, [("row", 0), ("class", 1), ("seat", 13)])]

main :: IO ()
main = do
    s <- readFile "input/16.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    -- putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
