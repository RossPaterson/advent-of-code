module Main where

import Utilities
import Graph
import Data.List

type Mana = Int
type HitPoints = Int
data State = State {
    boss_hit_points :: HitPoints,
    boss_damage :: HitPoints,
    player_hit_points :: HitPoints,
    player_armor :: HitPoints,
    mana_points :: Mana,
    active_spells :: [(Spell, Int)]
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
    active_spells = []
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
available_spells s = allSpells \\ map fst (active_spells s)

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
            active_spells = (spell, lifetime spell):active_spells s }) |
        spell <- available_spells s,
        cost spell <= mana_points s]

turns :: HitPoints -> State -> State
turns level =
    applySpells . mod_player_hit_points (subtract level) .
    bossAttack . applySpells

-- apply recurrent spells
applySpells :: State -> State
applySpells s
  | player_hit_points s > 0 = foldr (effect . fst) s' spells
  | otherwise = s'
  where
    s' = s {
        player_armor = 0,
        active_spells = [(spell, n-1) | (spell, n) <- spells, n > 1] }
    spells = active_spells s

bossAttack :: Effect
bossAttack s
  | boss_hit_points s > 0 = mod_player_hit_points (subtract damage) s
  | otherwise = s
  where
    damage = max 1 (boss_damage s - player_armor s)

initState :: State
initState = startState 50 500 55 8

testState :: State
testState = startState 10 250 13 8

testState2 :: State
testState2 = startState 10 250 14 8

solve :: Int -> State -> Int
solve level =
    fst . head . filter (success . snd) . shortestPaths (step level) .
    mod_player_hit_points (subtract level)

solve1 :: Int
solve1 = solve 0 initState

-- Part Two --

solve2 :: Int
solve2 = solve 1 initState

main :: IO ()
main = do
    print solve1
    print solve2
