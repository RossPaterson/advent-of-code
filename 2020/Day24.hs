module Main where

import CellularAutomaton
import Geometry
import Utilities
import Parser
import Control.Applicative
import Data.Char
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = [[Direction]]

data Direction = E | SE | SW | W | NW | NE
    deriving (Bounded, Enum, Show)

parse :: String -> Input
parse = map (runParser directions) . lines . map toUpper
  where
    directions = many enumValue

-- Part One

-- one step in the hexagonal tiling
direction :: Direction -> HexCoord
direction = hexDirection . fromEnum

-- tile reached in a sequence of steps
tile :: [Direction] -> HexCoord
tile = foldr (.+.) zero . map direction

-- tiles left flipped
blackTiles :: [[Direction]] -> [HexCoord]
blackTiles paths = [p | (p, n) <- frequency (map tile paths), odd n]

solve1 :: Input -> Int
solve1 = length . blackTiles

testInput :: String
testInput = "\
    \sesenwnenenewseeswwswswwnenewsewsw\n\
    \neeenesenwnwwswnenewnwwsewnenwseswesw\n\
    \seswneswswsenwwnwse\n\
    \nwnwneseeswswnenewneswwnewseswneseene\n\
    \swweswneswnenwsewnwneneseenw\n\
    \eesenwseswswnenwswnwnwsewwnwsene\n\
    \sewnenenenesenwsewnenwwwse\n\
    \wenwwweseeeweswwwnwwe\n\
    \wsweesenenewnwwnwsenewsenwwsesesenwne\n\
    \neeswseenwwswnwswswnw\n\
    \nenwswwsewswnenenewsenwsenwnesesenew\n\
    \enewnwewneswsewnwswenweswnenwsenwsw\n\
    \sweneswneswneneenwnewenewwneswswnese\n\
    \swwesenesewenwneswnwwneseswwne\n\
    \enesenwswwswneneswsenwnewswseenwsese\n\
    \wnwnesenesenenwwnenwsewesewsesesew\n\
    \nenewswnwewswnenesenwnesewesw\n\
    \eneswnwswnwsenenwnwnwwseeswneewsenese\n\
    \neswnwewnwnwseenwseesewsenwsweewe\n\
    \wseweeenwnesenwwwswnew\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 10)]

-- Part Two

neighbours :: HexCoord -> Set HexCoord
neighbours p = Set.fromList [p .+. direction dir | dir <- allValues]

-- black at next step?
rule :: Bool -> Int -> Bool
rule True blacks = blacks == 1 || blacks == 2
rule False blacks = blacks == 2

solve2 :: Input -> Int
solve2 =
    Set.size . times 100 (generation neighbours rule) .
        Set.fromList . blackTiles

tests2 :: [(String, Int)]
tests2 = [(testInput, 2208)]

main :: IO ()
main = do
    s <- readFile "input/24.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
