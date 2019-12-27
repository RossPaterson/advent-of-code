module Main where

import Intcode
import Data.List

-- Input processing

type Input = Memory

parse :: String -> Input
parse = readMemory

-- Part One

type Item = String
type Command = String

-- a tour of the ship (for my input), collecting all the safe items and
-- ending at Security Checkpoint
tour :: [Command]
tour = [
    "north",
    "west",
    "west",
    "take easter egg",
    "east",
    "take mug",
    "east",
    "south",
    "south",
    "south",
    "west",
    "north",
    "take jam",
    "south",
    "east",
    "north",
    "take asterisk",
    "east",
    "take klein bottle",
    "south",
    "west",
    "west",
    "take cake",
    "east",
    "take tambourine",
    "south",
    "east",
    "take polygon",
    "north"]

-- all items picked up of the tour
allItems :: [Item]
allItems = [drop 5 cmd | cmd <- tour, take 5 cmd == "take "]

-- Collect all the safe items, move to the Security Checkpoint and try to
-- get past the Pressure-Sensitive Floor by trying all subsets of items.
script :: [String]
script = tour ++ concat
    [map ("drop " ++) items ++ ["east"] ++ map ("take " ++) items |
        items <- subsequences allItems]

-- quoted strings in the input
quotes :: String -> [String]
quotes s = case dropWhile (/= '"') s of
    [] -> []
    _:t -> case span (/= '"') t of
        (_, []) -> error "Unbalanced quotes"
        (front, _:back) -> front : quotes back

-- Error messages from the PSF start with "Alert!".  Otherwise we have
-- a success message from the PSF, followed by a message with the password.
solve1 :: Input -> String
solve1 mem =
    head $ tail $ dropWhile ("Alert!" `isPrefixOf`) $ quotes $
        map fromValue . streamFunction mem . map toValue $ unlines $ script

-- there is no Part Two on Day 25

main :: IO ()
main = do
    s <- readFile "input/25.txt"
    let input = parse s
    putStrLn (solve1 input)
