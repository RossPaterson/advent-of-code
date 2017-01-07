module Main where

import Data.List

type Mana = Int
type HitPoints = Int
data State = State {
    boss_hit_points :: HitPoints,
    boss_damage :: HitPoints,
    player_hit_points :: HitPoints,
    player_armor :: HitPoints,
    mana_points :: Mana,
    active_spells :: [(Spell, Int)],
    available_spells :: [Spell]
    }

-- modifiers

mod_boss_hit_points :: (HitPoints -> HitPoints) -> State -> State
mod_boss_hit_points f s = s { boss_hit_points = f (boss_hit_points s) }

mod_player_hit_points :: (HitPoints -> HitPoints) -> State -> State
mod_player_hit_points f s = s { player_hit_points = f (player_hit_points s) }

mod_player_armor :: (HitPoints -> HitPoints) -> State -> State
mod_player_armor f s = s { player_armor = f (player_armor s) }

mod_mana_points :: (Mana -> Mana) -> State -> State
mod_mana_points f s = s { mana_points = f (mana_points s) }

-- initial states

startState :: HitPoints -> Mana -> HitPoints -> HitPoints -> State
startState player_hp mana boss_hp boss_damage = State {
    boss_hit_points = boss_hp,
    boss_damage = boss_damage,
    player_hit_points = player_hp,
    player_armor = 0,
    mana_points = mana,
    active_spells = [],
    available_spells = allSpells
    }

type Effect = State -> State

noEffect :: Effect
noEffect = id

data Spell = Spell {
    name :: String,
    cost :: Mana,
    immediateEffect :: Effect,
    lifetime :: Int,
    recurrentEffect :: Effect }

allSpells :: [Spell]
allSpells = [
    Spell "Magic Missile" 53 (mod_boss_hit_points (subtract 4)) 0 noEffect,
    Spell "Drain" 73
        (mod_boss_hit_points (subtract 2) . mod_player_hit_points (+2))
        0 noEffect,
    Spell "Shield" 113 noEffect 6 (mod_player_armor (+7)),
    Spell "Poison" 173 noEffect 6 (mod_boss_hit_points (subtract 3)),
    Spell "Recharge" 229 noEffect 5 (mod_mana_points (+101)) ]

bossAttack :: Effect
bossAttack s
  | boss_hit_points s > 0 = mod_player_hit_points (subtract damage) s
  | otherwise = s
  where
    damage = max 1 (boss_damage s - player_armor s)

applySpells :: State -> State
applySpells s
  | player_hit_points s > 0 =
    (foldr id s' (map (recurrentEffect . fst) spells)) {
        active_spells = [(spell, n-1) | (spell, n) <- live_spells],
        available_spells = map fst expired ++ available_spells s}
  | otherwise = s'
  where
    s' = s { player_armor = 0 }
    spells = active_spells s
    (live_spells, expired) = partition ((> 1) . snd) spells

afterSpell :: HitPoints -> Spell -> [Spell] -> State -> State
afterSpell level spell rest =
    applySpells . mod_player_hit_points (subtract level) .
    bossAttack . applySpells .
    immediateEffect spell . mod_mana_points (subtract (cost spell)) .
    startSpell spell rest

-- move recurrent spell to active list
startSpell :: Spell -> [Spell] -> State -> State
startSpell spell rest s
  | lifetime spell > 0 = s {
        active_spells = (spell, lifetime spell):active_spells s,
        available_spells = rest }
  | otherwise = s

-- states immediately before casting a spell
step :: HitPoints -> State -> [(Int, State)]
step level s = [(cost spell, afterSpell level spell rest s) |
        (spell, rest) <- pick (available_spells s),
        cost spell <= mana_points s ]

pick :: [a] -> [(a, [a])]
pick xs = [(x, front++back) | (front, x:back) <- zip (inits xs) (tails xs)]

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

initState :: State
initState = startState 50 500 55 8

testState :: State
testState = startState 10 250 13 8

testState2 :: State
testState2 = startState 10 250 14 8

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
