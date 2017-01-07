module Main where

import Utilities
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
  deriving Show

-- modifiers

mod_boss_hit_points :: (HitPoints -> HitPoints) -> State -> State
mod_boss_hit_points f s = s { boss_hit_points = f (boss_hit_points s) }

mod_player_hit_points :: (HitPoints -> HitPoints) -> State -> State
mod_player_hit_points f s = s { player_hit_points = f (player_hit_points s) }

mod_player_armor :: (HitPoints -> HitPoints) -> State -> State
mod_player_armor f s = s { player_armor = f (player_armor s) }

mod_mana_points :: (Mana -> Mana) -> State -> State
mod_mana_points f s = s { mana_points = f (mana_points s) }

mod_active_spells :: ([(Spell, Int)] -> [(Spell, Int)]) -> State -> State
mod_active_spells f s = s { active_spells = f (active_spells s) }

-- initial states

startState :: HitPoints -> Mana -> HitPoints -> HitPoints -> State
startState player_hp mana boss_hp boss_damage = State {
    boss_hit_points = boss_hp,
    boss_damage = boss_damage,
    player_hit_points = player_hp,
    player_armor = 0,
    mana_points = mana,
    active_spells = []
    }

initState :: State
initState = startState 50 500 55 8

testState :: State
testState = startState 10 250 13 8

testState2 :: State
testState2 = startState 10 250 14 8

data Spell
    = MagicMissile
    | Drain
    | Shield
    | Poison
    | Recharge
  deriving (Show, Eq, Bounded, Enum)

cost :: Spell -> Mana
cost MagicMissile = 53
cost Drain = 73
cost Shield = 113
cost Poison = 173
cost Recharge = 229

type Effect = State -> State

immediateEffect :: Spell -> Effect
immediateEffect MagicMissile = mod_boss_hit_points (subtract 4)
immediateEffect Drain =
    mod_boss_hit_points (subtract 2) . mod_player_hit_points (+2)
immediateEffect Shield = mod_active_spells ((Shield, 6):)
immediateEffect Poison = mod_active_spells ((Poison, 6):)
immediateEffect Recharge = mod_active_spells ((Recharge, 5):)

recurrentEffect :: Spell -> Effect
recurrentEffect Shield = mod_player_armor (+7)
recurrentEffect Poison = mod_boss_hit_points (subtract 3)
recurrentEffect Recharge = mod_mana_points (+101)
recurrentEffect _ = id

bossAttack :: Effect
bossAttack s
  | boss_hit_points s > 0 =
        mod_player_hit_points (subtract (max 1 (boss_damage s - player_armor s))) s
  | otherwise = s

applySpells :: State -> State
applySpells s
  | player_hit_points s > 0 =
    (foldr id s' (map (recurrentEffect . fst) spells)) {
        active_spells = [(spell, n-1) | (spell, n) <- spells, n > 1] }
  | otherwise = s'
  where
    s' = s { player_armor = 0 }
    spells = active_spells s

afterSpell :: HitPoints -> Spell -> State -> State
afterSpell level spell =
    applySpells . mod_player_hit_points (subtract level) .
    bossAttack . applySpells .
    immediateEffect spell . mod_mana_points (subtract (cost spell))

-- states immediately before casting a spell
step :: HitPoints -> State -> [(Int, State)]
step level s = [(cost spell, afterSpell level spell s) |
        spell <- allValues \\ map fst (active_spells s),
        cost spell <= mana_points s ]

success :: State -> Bool
success s = boss_hit_points s <= 0

failure :: State -> Bool
failure s = player_hit_points s <= 0

-- general search trees
data SearchTree a = Node a [(Int, SearchTree a)]

unfoldTree :: (a -> [(Int, a)]) -> a -> SearchTree a
unfoldTree f x = Node x [(d, unfoldTree f child) | (d, child) <- f x]

-- depth-first search, pruning branches that cost more than current best
dfs :: (a -> Bool) -> (a -> Bool) -> SearchTree a -> Int
dfs succeeded failed = search 0 maxBound
  where
    -- assume: sofar <= best
    search sofar best (Node x children)
      | sofar >= best || failed x = best::Int
      | succeeded x = sofar
      | otherwise =
        foldr id best [flip (search (sofar+n)) child | (n, child) <- children]

solve :: Int -> State -> Int
solve level =
    dfs success failure .  unfoldTree (step level) .
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
