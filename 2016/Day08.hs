module Main where

import Utilities
import Parser
import Control.Applicative
import Data.List

data Screen = Screen [[Bool]]
  deriving Show

showScreen :: Screen -> String
showScreen (Screen bits) = unlines (map (map showBit) bits)
  where
    showBit False = '.'
    showBit True = '#'

data Operation
    = Rect Int Int
    | RotateRow Int Int
    | RotateColumn Int Int
  deriving Show
type Input = [Operation]

parse :: String -> Input
parse = map (runParser operation) . lines
  where
    operation =
        Rect <$ string "rect " <*> nat <* char 'x' <*> nat <|>
        RotateRow <$ string "rotate row y=" <*> nat <* string " by " <*> nat <|>
        RotateColumn <$ string "rotate column x=" <*> nat <* string " by " <*> nat

screen :: Int -> Int -> Screen
screen w h = Screen (replicate h (replicate w False))

smallScreen :: Screen
smallScreen = screen 7 3

blankScreen :: Screen
blankScreen = screen 50 6

numLit :: Screen -> Int
numLit (Screen bits) = length (filter id (concat bits))

apply :: Screen -> Operation -> Screen
apply (Screen bits) (Rect x y) = Screen $
    updatePrefix y (updatePrefix x (const True)) bits
apply (Screen bits) (RotateRow y n) = Screen $
    updateAt y (rotate n) bits
apply (Screen bits) (RotateColumn x n) = Screen $
    transpose (updateAt x (rotate n) (transpose bits))

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt n f xs = take n xs ++ f (xs!!n) : drop (n+1) xs

updatePrefix :: Int -> (a -> a) -> [a] -> [a]
updatePrefix n f xs = map f (take n xs) ++ drop n xs

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = drop k xs ++ take k xs
  where k = length xs - n

solve1 :: Input -> Int
solve1 = numLit . foldl apply blankScreen

testInput :: String
testInput =
    "rect 3x2\n\
    \rotate column x=1 by 1\n\
    \rotate row y=0 by 4\n\
    \rotate column x=1 by 1\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 6)]

-- Part Two --

solve2 :: Input -> String
solve2 = showScreen . foldl apply blankScreen

main :: IO ()
main = do
    s <- readFile "input/08.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (solve2 input)
