module Main where

import Utilities
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.CompactSet (Set)
import qualified Data.CompactSet as Set

-- Input processing

type Input = [([Display], [Display])]
-- the order of segments in a display is irrelevant
type Display = Set Int
type Segment = Char

display :: String -> Display
display cs = Set.fromList [fromEnum c - fromEnum 'a' | c <- cs]

parse :: String -> Input
parse s =
    [(map display (words f), map display (words (drop 1 b))) |
        l <- lines s, let (f, b) = span (/= '|') l]

-- Part One

-- Digits shown on a seven-segment display:
--
--   -    .    -    -    .    -    -    -    -    -
--  | |  . |  . |  . |  | |  | .  | .  . |  | |  | |
--   .    .    -    -    -    -    -    .    -    -
--  | |  . |  | .  . |  . |  . |  | |  . |  | |  . |
--   -    .    -    -    .    -    -    .    -    -
--
digits :: [Display]
digits = map display [
    "abcefg", "cf", "acdeg", "acdfg", "bcdf",
    "abdfg", "abdefg", "acf", "abcdefg", "abcdfg"]

unique_lengths :: Set Int
unique_lengths =
    Set.fromList [s | (s, n) <- frequency (map Set.size digits), n == 1]

solve1 :: Input -> Int
solve1 =
    length . filter (`Set.member` unique_lengths) .
        map Set.size . concat . map snd

testInput :: String
testInput = "\
    \be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | \
    \fdgacbe cefdb cefbgd gcbe\n\
    \edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | \
    \fcgedb cgb dgebacf gc\n\
    \fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | \
    \cg cg fdcagb cbg\n\
    \fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | \
    \efabcd cedba gadfec cb\n\
    \aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | \
    \gecf egdcabf bgf bfgea\n\
    \fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | \
    \gebdcfa ecba ca fadegcb\n\
    \dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | \
    \cefg dcbef fcge gbcadfe\n\
    \bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | \
    \ed bcgafe cdgba cbgef\n\
    \egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | \
    \gbdfcae bgc cg cgb\n\
    \gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | \
    \fgae cfgab fg bagce\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 26)]

-- Part Two

type Digit = Char -- '0'..'9'
type Coding = Map Display Digit

-- determine the coding of the displays
get_coding :: [Display] -> Coding
get_coding ds = Map.fromList [
    (d0, '0'), (d1, '1'), (d2, '2'), (d3, '3'), (d4, '4'),
    (d5, '5'), (d6, '6'), (d7, '7'), (d8, '8'), (d9, '9')]
  where
    d1 = the [d | d <- ds, Set.size d == 2]
    d4 = the [d | d <- ds, Set.size d == 4]
    d7 = the [d | d <- ds, Set.size d == 3]
    d8 = the [d | d <- ds, Set.size d == 7]
    d0 = the [d | d <- ds, Set.size d == 6,
        Set.isSubsetOf d1 d && not (Set.isSubsetOf d4 d)]
    d6 = the [d | d <- ds, Set.size d == 6, not (Set.isSubsetOf d1 d)]
    d9 = the [d | d <- ds, Set.size d == 6, Set.isSubsetOf d4 d]
    d2 = the [d | d <- ds, Set.size d == 5,
        not (Set.isSubsetOf d1 d) && not (Set.isSubsetOf d d6)]
    d3 = the [d | d <- ds, Set.size d == 5, Set.isSubsetOf d1 d]
    d5 = the [d | d <- ds, Set.size d == 5, Set.isSubsetOf d d6]

the :: Show a => [a] -> a
the [] = error "no solution"
the [x] = x
the xs = error $ "multiple solutions: " ++ show xs

decode_entry :: ([Display], [Display]) -> Int
decode_entry (ds, ws) = read (map (get_coding ds!) ws)

solve2 :: Input -> Int
solve2 = sum . map decode_entry

tests2 :: [(String, Int)]
tests2 = [(testInput, 61229)]

main :: IO ()
main = do
    s <- readFile "input/08.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
