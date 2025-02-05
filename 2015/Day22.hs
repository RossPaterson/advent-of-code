module Main where

import Utilities
import Graph
import Data.Map (Map)
import qualified Data.Map as Map

-- Input processing

type Input = (Int, Int)

parse :: String -> Input
parse s = case readNumbers s of
    [hp, d] -> (hp, d)
    _ -> error "bad input"

-- Part One

type Mana = Int
type HitPoints = Int
data State = State {
    boss_hit_points :: HitPoints,
    boss_damage :: HitPoints,
    player_hit_points :: HitPoints,
    player_armor :: HitPoints,
    mana_points :: Mana,
    active_spells :: Map Spell Int
    }
    deriving (Eq, Ord)
data Spell = MagicMissile | Drain | Shield | Poison | Recharge
    deriving (Bounded, Enum, Eq, Ord, Show)

-- modifiers

mod_boss_hit_points :: (HitPoints -> HitPoints) -> State -> State
mod_boss_hit_points f s = s { boss_hit_points = f (boss_hit_points s) }

mod_player_hit_points :: (HitPoints -> HitPoints) -> State -> State
mod_player_hit_points f s = s { player_hit_points = f (player_hit_points s) }

mod_player_armor :: (HitPoints -> HitPoints) -> State -> State
mod_player_armor f s = s { player_armor = f (player_armor s) }

mod_mana_points :: (Mana -> Mana) -> State -> State
mod_mana_points f s = s { mana_points = f (mana_points s) }

success :: State -> Bool
success s = boss_hit_points s <= 0

-- initial states

startState :: HitPoints -> Mana -> HitPoints -> HitPoints -> State
startState player_hp mana boss_hp boss_dmg = State {
    boss_hit_points = boss_hp,
    boss_damage = boss_dmg,
    player_hit_points = player_hp,
    player_armor = 0,
    mana_points = mana,
    active_spells = Map.empty
    }

cost :: Spell -> Mana
cost MagicMissile = 53
cost Drain = 73
cost Shield = 113
cost Poison = 173
cost Recharge = 229

lifetime :: Spell -> Int
lifetime MagicMissile = 1
lifetime Drain = 1
lifetime Shield = 6
lifetime Poison = 6
lifetime Recharge = 5

effect :: Spell -> State -> State
effect MagicMissile = mod_boss_hit_points (subtract 4)
effect Drain = mod_boss_hit_points (subtract 2) . mod_player_hit_points (+2)
effect Shield = mod_player_armor (+7)
effect Poison = mod_boss_hit_points (subtract 3)
effect Recharge = mod_mana_points (+101)

type Effect = State -> State

allSpells :: [Spell]
allSpells = allValues

available_spells :: State -> [Spell]
available_spells s =
    [spell | spell <- allSpells, not (Map.member spell (active_spells s))]

-- states immediately before casting a spell
step :: HitPoints -> State -> [(Int, State)]
step level s =
    [(cost spell, turns level s') |
        boss_hit_points s > 0 && player_hit_points s > 0,
        (spell, s') <- castSpell s ]

-- spells that can be cast from the current state
castSpell :: State -> [(Spell, State)]
castSpell s =
    [(spell, s {
            mana_points = mana_points s - cost spell,
            active_spells = Map.insert spell (lifetime spell) (active_spells s)
        }) |
        spell <- available_spells s,
        cost spell <= mana_points s]

turns :: HitPoints -> State -> State
turns level =
    applySpells . mod_player_hit_points (subtract level) .
    bossAttack . applySpells

-- apply recurrent spells
applySpells :: State -> State
applySpells s
  | player_hit_points s > 0 =
        foldr (effect . fst) s' (Map.assocs (active_spells s))
  | otherwise = s'
  where
    s' = s {
        player_armor = 0,
        active_spells = fmap (subtract 1) (Map.filter (> 1) (active_spells s))
        }

bossAttack :: Effect
bossAttack s
  | boss_hit_points s > 0 = mod_player_hit_points (subtract damage) s
  | otherwise = s
  where
    damage = max 1 (boss_damage s - player_armor s)

initState :: Int -> Int -> State
initState h d = startState 50 500 h d

testState :: State
testState = startState 10 250 13 8

testState2 :: State
testState2 = startState 10 250 14 8

solve :: Int -> State -> Int
solve level s0 =
    head [t |
        (t, s) <- shortestPaths (step level)
            [mod_player_hit_points (subtract level) s0],
        success s]

solve1 :: Input -> Int
solve1 (h, d) = solve 0 (initState h d)

-- Part Two --

solve2 :: Input -> Int
solve2 (h, d) = solve 1 (initState h d)

main :: IO ()
main = do
    s <- readFile "input/22.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
