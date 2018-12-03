module Main where

import Parser
import Control.Applicative
import Data.List
import Data.Maybe

data Operation
    = SwapPosition Int Int
    | SwapLetter Char Char
    | Rotate Rotation
    | Reverse Int Int
    | Move Int Int
  deriving Show
data Rotation
    = LeftSteps Int
    | RightSteps Int
    | LetterPos Char
  deriving Show
type Input = [Operation]

parse :: String -> Input
parse = map (runParser operation) . lines
  where
    operation =
        SwapPosition <$ string "swap position " <*> int <* string " with position " <*> int <|>
        SwapLetter <$ string "swap letter " <*> anyChar <* string " with letter " <*> anyChar <|>
        Rotate <$ string "rotate " <*> rotation <|>
        Reverse <$ string "reverse positions " <*> int <* string " through " <*> int <|>
        Move <$ string "move position " <*> int <* string " to position " <*> int
    rotation =
        LeftSteps <$ string "left " <*> int <* string " step" <* plural <|>
        RightSteps <$ string "right " <*> int <* string " step" <* plural <|>
        LetterPos <$ string "based on position of letter " <*> anyChar
    plural = optional (char 's')

apply :: String -> Operation -> String
apply s (SwapPosition x y)
  | x < y = swapPos x y s
  | y < x = swapPos y x s
  | otherwise = s
apply s (SwapLetter x y) = swapLetters x y s
apply s (Rotate (LeftSteps x)) = rotateLeft x s
apply s (Rotate (RightSteps x)) = rotateRight x s
apply s (Rotate (LetterPos x)) =
    rotateRight (rotationRule (fromJust (elemIndex x s))) s
apply s (Reverse x y)
  | x < y = reversePos x y s
  | otherwise = reversePos y x s
apply s (Move x y) = move x y s

rotationRule :: Int -> Int
rotationRule i = i + 1 + fromEnum (i >= 4)

-- assumes x < y
swapPos :: Int -> Int -> String -> String
swapPos x y s = sa ++ [cy] ++ sb ++ [cx] ++ sc
  where
    (sa, cx:sbc) = splitAt x s
    (sb, cy:sc) = splitAt (y-x-1) sbc

swapLetters :: Char -> Char -> String -> String
swapLetters x y s
  | px < py = swapPos px py s
  | px > py = swapPos py px s
  | otherwise = s
  where
    px = fromJust (elemIndex x s)
    py = fromJust (elemIndex y s)

rotateRight :: Int -> [a] -> [a]
rotateRight n = rotateLeft (-n)

rotateLeft :: Int -> [a] -> [a]
rotateLeft n xs = back ++ front
  where
    (front, back) = splitAt (n `mod` length xs) xs

-- assumes x < y
reversePos :: Int -> Int -> [a] -> [a]
reversePos x y s = sa ++ reverse sb ++ sc
  where
    (sa, sbc) = splitAt x s
    (sb, sc) = splitAt (y-x+1) sbc

move :: Int -> Int -> [a] -> [a]
move x y s = front' ++ [cx] ++ back'
  where
    (front, cx:back) = splitAt x s
    (front', back') = splitAt y (front++back)

text = "abcdefgh"

solve1 :: Input -> String
solve1 = foldl apply text

test = "swap position 4 with position 0\nswap letter d with letter b\nreverse positions 0 through 4\nrotate left 1 step\nmove position 1 to position 4\nmove position 3 to position 0\nrotate based on position of letter b\nrotate based on position of letter d\n"

-- Part Two --

invert :: String -> Operation -> String
invert s (SwapPosition x y)
  | x < y = swapPos x y s
  | y < x = swapPos y x s
  | otherwise = s
invert s (SwapLetter x y) = swapLetters x y s
invert s (Rotate (LeftSteps x)) = rotateRight x s
invert s (Rotate (RightSteps x)) = rotateLeft x s
invert s (Rotate (LetterPos x)) =
    findRotation (flip apply (Rotate (LetterPos x))) s
invert s (Reverse x y)
  | x < y = reversePos x y s
  | otherwise = reversePos y x s
invert s (Move x y) = move y x s

-- find a rotation that is the inverse of f on s (assumes there is exactly one)
findRotation :: (String -> String) -> String -> String
findRotation f s =
   head [s' | i <- [0..length s-1], let s' = rotateLeft i s, f s' == s]

text2 = "fbgdceah"

solve2 :: Input -> String
solve2 = foldr (flip invert) text2

main :: IO ()
main = do
    s <- readFile "input21.txt"
    let input = parse s
    putStrLn (solve1 input)
    putStrLn (solve2 input)
