module Main where

import Parser
import Control.Applicative

data Coord = Coord Int Int
  deriving (Show, Eq, Ord)
data Instruction
    = TurnOn Coord Coord
    | Toggle Coord Coord
    | TurnOff Coord Coord
  deriving Show
type Input = [Instruction]

parse :: String -> Input
parse = map (runParser instruction) . lines
  where
    instruction =
        TurnOn <$ string "turn on " <*> coord <* string " through " <*> coord <|>
        Toggle <$ string "toggle " <*> coord <* string " through " <*> coord <|>
        TurnOff <$ string "turn off " <*> coord <* string " through " <*> coord
    coord = Coord <$> nat <* char ',' <*> nat

type Grid a = [[a]]

initGrid :: a -> Grid a
initGrid x = replicate 1000 (replicate 1000 x)

switch :: Grid Bool -> Instruction -> Grid Bool
switch lights (TurnOn topleft botright) =
    updateRange topleft botright (const True) lights
switch lights (Toggle topleft botright) =
    updateRange topleft botright not lights
switch lights (TurnOff topleft botright) =
    updateRange topleft botright (const False) lights

updateRange :: Coord -> Coord -> (a -> a) -> Grid a -> Grid a
updateRange (Coord x1 y1) (Coord x2 y2) f lights =
    top ++ map alter mid ++ bot
  where
    (top, mid_bot) = splitAt y1 lights
    (mid, bot) = splitAt (y2-y1+1) mid_bot
    alter line = l ++ map f m ++ r
      where
        (l, mr) = splitAt x1 line
        (m, r) = splitAt (x2-x1+1) mr

solve1 :: Input -> Int
solve1 = length . filter id . concat . foldl switch (initGrid False)

-- Part Two --

switch2 :: Grid Int -> Instruction -> Grid Int
switch2 lights (TurnOn topleft botright) =
    updateRange topleft botright (+1) lights
switch2 lights (Toggle topleft botright) =
    updateRange topleft botright (+2) lights
switch2 lights (TurnOff topleft botright) =
    updateRange topleft botright (max 0 . subtract 1) lights

solve2 :: Input -> Int
solve2 = sum . concat . foldl switch2 (initGrid 0)

main :: IO ()
main = do
    s <- readFile "input06.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
