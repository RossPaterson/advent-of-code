module Main where

import Utilities
import Data.List

-- Input processing

type Input = [Layer Char]

type Layer a = [[a]]

parse :: Int -> Int -> String -> Input
parse w h = takes h . takes w . filter (/= '\n')

-- Part One

-- number of occurrences of a value in a layer
countPixel :: Eq a => a -> Layer a -> Int
countPixel c = sum . map (length . filter (== c))

solve1 :: Input -> Int
solve1 s = head [countPixel '1' l * countPixel '2' l |
    l <- leastBy (countPixel '0') s]

testInput1 :: String
testInput1 = "123456789012"

tests1 :: [(String, [Layer Char])]
tests1 = [(testInput1, [["123", "456"], ["789", "012"]])]

-- Part Two

combine :: [Layer Char] -> Layer Char
combine = map (map topVisible . transpose) . transpose

topVisible :: [Char] -> Char
topVisible = head . dropWhile (== '2') -- '2' is transparent

display :: Char -> Char
display '1' = '@'
display _ = ' '

solve2 :: Input -> Layer Char
solve2 = map (map display) . combine

testInput2 :: String
testInput2 = "0222112222120000"

tests2 :: [(String, Layer Char)]
tests2 = [(testInput2, ["01", "10"])]

main :: IO ()
main = do
    s <- readFile "input/08.txt"
    let input = parse 25 6 s
    putStr (unlines (failures "solve1" (parse 3 2) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (combine . parse 2 2) tests2))
    putStr (unlines (solve2 input))
