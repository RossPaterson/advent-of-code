module Main where

import Parser
import Utilities
import Control.Applicative
import Data.Char

data Room = Room { name :: [String], sector :: Int, checksum :: String }
    deriving Show
type Input = [Room]

parse :: String -> Input
parse = map (runParser room) . lines
  where
    room = Room <$>
        (some letter `sepBy1` char '-')
        <* char '-' <*> nat <* char '[' <*> some letter <* char ']'

solve1 :: Input -> Int
solve1 = sum . map sector . filter real_room

real_checksum :: [String] -> String
real_checksum = take 5 . mostCommon . concat

real_room :: Room -> Bool
real_room rm = checksum rm == real_checksum (name rm)

tests1 :: [(String, Bool)]
tests1 = [
    ("aaaaa-bbb-z-y-x-123[abxyz]", True),
    ("a-b-c-d-e-f-g-h-987[abcde]", True),
    ("not-a-real-room-404[oarel]", True),
    ("totally-real-room-200[decoy]", False)]

-- Part Two --

solve2 :: Input -> Int
solve2 rs = head [sector r | r <- filter real_room rs, decrypt r == "northpole object storage"]

shiftLetter :: Int -> Char -> Char
shiftLetter n c = chr ((ord c - ord 'a' + n) `mod` 26 + ord 'a')

decrypt :: Room -> String
decrypt (Room ns s _) = unwords (map (map (shiftLetter s)) ns)

tests2 :: [(String, String)]
tests2 = [("qzmt-zixmtkozy-ivhz-343[x]", "very encrypted name")]

main :: IO ()
main = do
    s <- readFile "input/04.txt"
    let input = parse s
    putStr (unlines (failures "real_room" (real_room . head . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "decrypt" (decrypt . head . parse) tests2))
    print (solve2 input)
