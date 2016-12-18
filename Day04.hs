module Day04 where

import Parser
import Utilities
import Control.Applicative
import Data.Char

data Room = Room { name :: [String], sector :: Int, checksum :: String }
    deriving Show
type Input = [Room]

parse :: String -> Input
parse = map getRoom . lines

getRoom :: String -> Room
getRoom = runParser $ Room <$>
    (some letter `sepBy1` char '-')
    <* char '-' <*> nat <* char '[' <*> some letter <* char ']'

solve1 :: Input -> Int
solve1 = sum . map sector . filter real_room

real_checksum :: [String] -> String
real_checksum = take 5 . mostCommon . concat

real_room :: Room -> Bool
real_room rm = checksum rm == real_checksum (name rm)

-- Part Two --

solve2 :: Input -> Int
solve2 rs = head [sector r | r <- filter real_room rs, decrypt r == "northpole object storage"]

shiftLetter :: Int -> Char -> Char
shiftLetter n c = chr ((ord c - ord 'a' + n) `mod` 26 + ord 'a')

decrypt :: Room -> String
decrypt (Room ns s c) = unwords (map (map (shiftLetter s)) ns)

puzzle1 = do
    s <- readFile "input04.txt"
    print (solve1 (parse s))

puzzle2 = do
    s <- readFile "input04.txt"
    print (solve2 (parse s))

test1 = "aaaaa-bbb-z-y-x-123[abxyz]\na-b-c-d-e-f-g-h-987[abcde]\nnot-a-real-room-404[oarel]\ntotally-real-room-200[decoy]"
test2 = Room ["qzmt", "zixmtkozy", "ivhz"] 343 ""
