module Main where

import Parser
import Utilities

-- Input processing

type Input = [(Int, Int)]

parse :: String -> Input
parse =
    map (runParser range) . lines .
        map (\c -> if c == ',' then '\n' else c) . filter (/= '\n')
  where
    range = (,) <$> nat <* char '-' <*> nat

-- Part One

solve1 :: Input -> Int
solve1 = sum . filter invalid . concatMap expandRange

expandRange :: (Int, Int) -> [Int]
expandRange (lo, hi) = [lo..hi]

invalid :: Int -> Bool
invalid n = even len && front == back
  where
    cs = show n
    len = length cs
    (front, back) = splitAt (len `div` 2) cs

testInput :: String
testInput = "\
    \11-22,95-115,998-1012,1188511880-1188511890,222220-222224,\n\
    \1698522-1698528,446443-446449,38593856-38593862,565653-565659,\n\
    \824824821-824824827,2121212118-2121212124\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 1227775554)]

-- Part Two

solve2 :: Input -> Int
solve2 = sum . filter invalid2 . concatMap expandRange

invalid2 :: Int -> Bool
invalid2 n =
    not $ null [f | f <- properFactors len, allSame (takes f cs)]
  where
    cs = show n
    len = length cs

properFactors :: Int -> [Int]
properFactors n = [f | f <- [1..n `div` 2], n `mod` f == 0]

allSame :: Eq a => [a] -> Bool
allSame [] = True
allSame (x:xs) = and (zipWith (==) xs (x:xs))

tests2 :: [(String, Int)]
tests2 = [(testInput, 4174379265)]

main :: IO ()
main = do
    s <- readFile "input/02.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
