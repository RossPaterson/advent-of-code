module Main where

import Parser
import Control.Applicative
import Data.List

data Range = Range { low :: Integer, high :: Integer }
  deriving (Show, Eq, Ord)
type Input = [Range]

parse :: String -> Input
parse = map getRange . lines

getRange :: String -> Range
getRange = runParser $ Range <$> natural <* char '-' <*> natural

-- given ranges in order, merge overlapping and contiguous ranges
mergeRanges :: [Range] -> [Range]
mergeRanges [] = []
mergeRanges (Range lo hi:rs) = mergeAux lo hi rs

mergeAux :: Integer -> Integer -> [Range] -> [Range]
mergeAux lo hi [] = [Range lo hi]
mergeAux lo hi (Range lo2 hi2:rs)
  | lo2 <= hi+1 = mergeAux lo (max hi hi2) rs
  | otherwise = Range lo hi : mergeAux lo2 hi2 rs

solve1 :: Input -> Integer
solve1 = (+1) . high . head . mergeRanges . sort

test = "5-8\n0-2\n4-7\n"

-- Part Two --

sizeRange :: Range -> Integer
sizeRange (Range lo hi) = hi - lo + 1

solve2 :: Input -> Integer
solve2 = (2^32 -) . sum . map sizeRange . mergeRanges . sort

main :: IO ()
main = do
    s <- readFile "input20.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
