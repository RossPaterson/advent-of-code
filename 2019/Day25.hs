module Main where

import Intcode
import Data.List
import Data.Maybe

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

-- Given a game positioned at Security Checkpoint and carrying all the
-- safe items, try to get past the Pressure-Sensitive Floor by dropping
-- the specified items.
tryDropping :: Automaton -> [Item] -> Maybe String
tryDropping a items = case quotes output of
    response:rest
      | not ("Alert!" `isPrefixOf` response) -> Just (head rest)
      | otherwise -> Nothing
    [] -> error "No response"
  where
    input = unlines (map ("drop " ++) items ++ ["east"])
    output = map fromValue $ fst $ runPartial a $ map toValue $ input

dropAll :: [Command]
dropAll = map ("drop " ++) allItems

-- quoted strings in the input
quotes :: String -> [String]
quotes s = case dropWhile (/= '"') s of
    [] -> []
    _:t -> case span (/= '"') t of
        (_, []) -> error "Unbalanced quotes"
        (front, _:back) -> front : quotes back

solve1 :: Input -> String
solve1 mem = head $ mapMaybe (tryDropping checkpoint) $ subsequences allItems
  where
    checkpoint = snd $ runPartial (automaton mem) $ map toValue $ unlines tour

-- there is no Part Two on Day 25

main :: IO ()
main = do
    s <- readFile "input/25.txt"
    let input = parse s
    putStrLn (solve1 input)
