module Main where

import Geometry
import Parser
import Utilities
import Control.Applicative
import Data.Maybe

-- Input processing

type Input = Almanac
type Almanac = ([Int], [(String, String, [MapEntry])])

data MapEntry = MapEntry {
    dst_range_start :: Int,
    src_range_start :: Int,
    range_length :: Int
    }
    deriving (Show)

parse :: String -> Input
parse s = (runParser seeds (head paras), map mkmap (tail paras))
  where
    paras = paragraphs s
    seeds = string "seeds: " *> nat `sepBy1` space <* char '\n'
    mkmap para = case lines para of
        [] -> error "empty map"
        (hdr:entries) ->
            case runParser header hdr of
                (source, target) ->
                    (source, target, map (runParser mapentry) entries)
    header = (,) <$> word <* string "-to-" <*> word <* string " map:"
    word = some letter
    mapentry = MapEntry <$> nat <* space <*> nat <* space <*> nat

-- Ranges (NB: different representation to the problem statement)

type Range = AABox Int

range :: Int -> Int -> Range
range lo size = boundingBox [lo, lo + size - 1]

-- the source range of a map entry
src_range :: MapEntry -> Range
src_range e = range (src_range_start e) (range_length e)

-- Part One

-- the strings in each map are ignored
map_entries :: (String, String, [MapEntry]) -> [MapEntry]
map_entries (_, _, es) = es

-- how much entries in range are shifted
offset :: MapEntry -> Int
offset e = dst_range_start e - src_range_start e

-- the number is shifted by the first entry for which it is in range
map_number :: Int -> [MapEntry] -> Int
map_number src es =
    head ([src + offset e | e <- es, inBox src (src_range e)] ++ [src])

solve1 :: Input -> Int
solve1 (seeds, maps) =
    minimum [foldl map_number s ((map map_entries) maps) | s <- seeds]

testInput :: String
testInput = "\
    \seeds: 79 14 55 13\n\
    \\n\
    \seed-to-soil map:\n\
    \50 98 2\n\
    \52 50 48\n\
    \\n\
    \soil-to-fertilizer map:\n\
    \0 15 37\n\
    \37 52 2\n\
    \39 0 15\n\
    \\n\
    \fertilizer-to-water map:\n\
    \49 53 8\n\
    \0 11 42\n\
    \42 0 7\n\
    \57 7 4\n\
    \\n\
    \water-to-light map:\n\
    \88 18 7\n\
    \18 25 70\n\
    \\n\
    \light-to-temperature map:\n\
    \45 77 23\n\
    \81 45 19\n\
    \68 64 13\n\
    \\n\
    \temperature-to-humidity map:\n\
    \0 69 1\n\
    \1 0 69\n\
    \\n\
    \humidity-to-location map:\n\
    \60 56 37\n\
    \56 93 4\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 35)]

-- Part Two

-- mapping ranges may involve splitting them
map_range :: [Range] -> [MapEntry] -> [Range]
map_range rs [] = rs -- unmapped ranges
map_range rs (e:es) =
    -- overlaps are mapped by this entry
    map (shiftBox (offset e)) (catMaybes [intersectBox r e_range | r <- rs]) ++
    -- residues may be mapped by later entries
    map_range [r' | r <- rs, r' <- diffBox r e_range] es
  where
    e_range = src_range e

-- the seeds list was actually a list of ranges
ranges :: [Int] -> [Range]
ranges ns = [range start size | [start, size] <- takes 2 ns]

solve2 :: Input -> Int
solve2 (seeds, maps) =
    minimum $ map minCorner $
        foldl map_range (ranges seeds) $ map map_entries maps

tests2 :: [(String, Int)]
tests2 = [(testInput, 46)]

main :: IO ()
main = do
    s <- readFile "input/05.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
