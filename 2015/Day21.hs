module Main where

import Utilities

-- Input processing

type Input = PlayerState

data PlayerState = Player {
    hit_points :: Int,
    damage_score :: Int,
    armor_score :: Int
    }

parse :: String -> Input
parse s = case readNumbers s of
    [h, d, a] -> Player h d a
    _ -> error "bad input"

-- Part One

data Item = Item {
    name :: String,
    cost :: Int,
    damage :: Int,
    armor :: Int
    }
  deriving Show

weapon_items :: [Item]
weapon_items = [
    Item "Dagger"        8     4       0,
    Item "Shortsword"   10     5       0,
    Item "Warhammer"    25     6       0,
    Item "Longsword"    40     7       0,
    Item "Greataxe"     74     8       0]

armor_items :: [Item]
armor_items = [
    Item "Leather"      13     0       1,
    Item "Chainmail"    31     0       2,
    Item "Splintmail"   53     0       3,
    Item "Bandedmail"   75     0       4,
    Item "Platemail"   102     0       5]

ring_items :: [Item]
ring_items = [
    Item "Damage +1"    25     1       0,
    Item "Damage +2"    50     2       0,
    Item "Damage +3"   100     3       0,
    Item "Defense +1"   20     0       1,
    Item "Defense +2"   40     0       2,
    Item "Defense +3"   80     0       3]

initHP :: Int
initHP = 100

total :: (a -> Int) -> [a] -> Int
total f = sum . map f

initState :: [Item] -> PlayerState
initState is = Player initHP (total damage is) (total armor is)

purchases :: [[Item]]
purchases = [ws ++ as ++ rs |
    ws <- chooseBetween 1 1 weapon_items,
    as <- chooseBetween 0 1 armor_items,
    rs <- chooseBetween 0 2 ring_items]

-- first player is about to attack
type State = (PlayerState, PlayerState)

finished :: State -> Bool
finished (a, _) = hit_points a <= 0

attack :: State -> State
attack (a, d) = (d { hit_points = hp }, a)
  where
    hp = hit_points d - max 1 (damage_score a - armor_score d)

beats :: PlayerState -> PlayerState -> Bool
beats p1 p2 =
    odd $ length $ takeWhile (not . finished) $ iterate attack (p1, p2)

solve1 :: PlayerState -> Int
solve1 boss =
    minimum [total cost is | is <- purchases, beats (initState is) boss]

-- Part Two --

solve2 :: PlayerState -> Int
solve2 boss =
    maximum [total cost is | is <- purchases, beats boss (initState is)]

main :: IO ()
main = do
    s <- readFile "input/21.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
