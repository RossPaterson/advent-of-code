module Main where

import Parser
import Utilities
import Control.Applicative
import Data.Char
import Data.List
import Data.Maybe
import Data.Ord
import Data.Map (Map, (!))
import qualified Data.Map as Map

-- Input processing

type Input = State

data State = State { immuneSystem :: Army, infection :: Army }
-- labels for the two armies
data Side = ImmuneSystem | Infection
  deriving (Show, Enum, Bounded)

-- the army for each side consists of several groups
type Army = Map GroupNo Group
type GroupNo = Int
-- a group is identified by the side of its army and a number within that army
type GroupID = (Side, GroupNo)

-- a group consists of some number of usits of the same type
data Group = Group { numUnits :: Int, unitType :: UnitType }
  deriving Show

-- properties of a type of unit
data UnitType = UnitType {
    hitPoints :: Int,
    weaknesses :: [AttackType],
    immunities :: [AttackType],
    attackDamage :: Int,
    attackType :: AttackType,
    initiative :: Int
    }
  deriving Show

-- types of attack
data AttackType = Fire | Cold | Slashing | Bludgeoning | Radiation
  deriving (Show, Eq, Enum, Bounded)

parse :: String -> Input
parse s =
    State {
        immuneSystem =
            makeArmy (map (runParser unit_group) (tail immune_system_lines)),
        infection =
            makeArmy (map (runParser unit_group) (tail infection_lines))
    }
  where
    ls = lines s
    (immune_system_lines, "":infection_lines) = span (not . null) ls
    unit_group =
        Group <$> nat <* string " units each with " <*> unit_type
    unit_type =
        mkUnitType <$> nat <* string " hit points " <*> properties <*
            string "with an attack that does " <*> nat <* char ' ' <*>
            attack_type <* string " damage at initiative " <*> nat
    properties = optional $ char '(' *> props <* string ") "
    props =
        (,) <$> weak_to <*>
            (fromMaybe [] <$> optional (string "; " *> immune_to)) <|>
        flip (,) <$> immune_to <*>
            (fromMaybe [] <$> optional (string "; " *> weak_to))
    weak_to = string "weak to " *> sepBy1 attack_type (string ", ")
    immune_to = string "immune to " *> sepBy1 attack_type (string ", ")
    attack_type =
        foldr1 (<|>) [c <$ string (map toLower (show c)) | c <- allValues]
    mkUnitType hp Nothing = UnitType hp [] []
    mkUnitType hp (Just (ws, is)) = UnitType hp ws is

-- Number the groups of an army
makeArmy :: [Group] -> Army
makeArmy gs = Map.fromList (zip [1..] gs)

-- extract an army by side name
get :: Side -> State -> Army
get ImmuneSystem = immuneSystem
get Infection = infection

-- update an army by side name
set :: Side -> Army -> State -> State
set ImmuneSystem army s = s { immuneSystem = army }
set Infection army s = s { infection = army }

-- Part One

-- the number of survivors of a complete battle
solve1 :: Input -> Int
solve1 = size . battle

-- the total number of units in a game state
size :: State -> Int
size s = sizeArmy (immuneSystem s) + sizeArmy (infection s)

-- the total number of units in an army
sizeArmy :: Army -> Int
sizeArmy = sum . map numUnits . Map.elems

-- battle until a steady state is reached, which can be because one side
-- is eliminated or neither side is not able to damage the other, either
-- because they are immune to the weapon used, or the attacker's damage
-- is less than a single unit's hit points.
battle :: State -> State
battle = convergeBy (same size) . iterate fight

-- one round of battle
fight :: State -> State
fight s = foldl attack s $ sortBy (attackOrder s) $ targetSelection s

type Attack = (GroupID, GroupID)

-- all attacks for one round of battle
targetSelection :: State -> [Attack]
targetSelection s =
    [((side, a), (opponent side, t)) |
        side <- allValues,
        (a, t) <- armyTargets (get side s) (get (opponent side) s)]

-- order in which attacks (by both armies) take place in one round
attackOrder :: State -> Attack -> Attack -> Ordering
attackOrder s =
    comparing (Down . initiative . unitType . lookupGroup s . fst)

-- targets for attacks by groups of one army on another
armyTargets :: Army -> Army -> [(GroupNo, GroupNo)]
armyTargets attackers defenders =
    sel (sortBy (selectionOrder attackers) attacker_nos) defender_nos
  where
    attacker_nos = Map.keys attackers
    defender_nos = Map.keys defenders
    sel [] _ = []
    sel (a:as) ds = case mb_target of
        Nothing -> sel as rest
        Just target -> (a, target):sel as rest
      where
        (mb_target, rest) = selectTarget (attackers!a) defenders ds

-- order in which groups of the same army select their targets
selectionOrder :: Army -> GroupNo -> GroupNo -> Ordering
selectionOrder army =
    comparing (Down . effectivePower . (army!)) <>
    comparing (Down . initiative . unitType. (army!))

-- select a target for a group from the untargeted groups of the
-- opposing army
selectTarget ::
    Group -> Army -> [GroupNo] -> (Maybe GroupNo, [GroupNo])
selectTarget attack_group defend_army defenders = case targets of
    g:gs | totalDamage attack_group (defend_army!g) > 0 -> (Just g, gs)
    _ -> (Nothing, defenders)
  where
    targets = sortBy (targetPriority attack_group defend_army) defenders

-- targeting priority for an attacking group of groups within the
-- defending army
targetPriority :: Group -> Army -> GroupNo -> GroupNo -> Ordering
targetPriority attack_group defend_army =
    comparing (Down . totalDamage attack_group . (defend_army!)) <>
        comparing (Down . effectivePower . (defend_army!)) <>
        comparing (Down . initiative . unitType . (defend_army!))

-- the damage one group could do to another, if the defending group
-- had enough hit points
totalDamage :: Group -> Group -> Int
totalDamage attack_group defend_group
  | att `elem` immunities (unitType defend_group) = 0
  | att `elem` weaknesses (unitType defend_group) = 2*default_damage
  | otherwise = default_damage
  where
    att = attackType (unitType attack_group)
    default_damage = effectivePower attack_group

-- the default power of a group's attack
effectivePower :: Group -> Int
effectivePower (Group n uts) = n * attackDamage uts

-- execute an attack (if both participants are still alive)
attack :: State -> Attack -> State
attack s ((aside, anum), (dside, dnum)) = set dside defend_army' s
  where
    attack_army = get aside s
    defend_army = get dside s
    defend_army' = case Map.lookup anum attack_army of
        Nothing -> defend_army
        Just attack_unit -> case Map.lookup dnum defend_army of
            Nothing -> defend_army
            Just defend_unit
              | n' <= 0 -> Map.delete dnum defend_army
              | otherwise -> Map.insert dnum (Group n' ut) defend_army
              where
                damage = totalDamage attack_unit defend_unit
                Group n ut = defend_unit
                n' = n - damage `div` hitPoints ut

-- the identified group within the state
lookupGroup :: State -> GroupID -> Group
lookupGroup s (side, i) = get side s!i

-- the other side of the battle
opponent :: Side -> Side
opponent Infection = ImmuneSystem
opponent ImmuneSystem = Infection

tests1 :: [(String, Int)]
tests1 = [(testInput, 5216)]

testInput :: String
testInput =
    "Immune System:\n\
    \17 units each with 5390 hit points (weak to radiation, bludgeoning) with an attack that does 4507 fire damage at initiative 2\n\
    \989 units each with 1274 hit points (immune to fire; weak to bludgeoning, slashing) with an attack that does 25 slashing damage at initiative 3\n\
    \\n\
    \Infection:\n\
    \801 units each with 4706 hit points (weak to radiation) with an attack that does 116 bludgeoning damage at initiative 1\n\
    \4485 units each with 2961 hit points (immune to radiation; weak to fire, cold) with an attack that does 12 slashing damage at initiative 4\n"

-- Part Two

-- the number of survivors after boosting the immune system by the
-- minimum amount needed for it to win the battle.
-- uses linear search of increasing boosts, because the behaviour is
-- not monotonic
solve2 :: Input -> Int
solve2 s = head $ [size s' |
        v <- [1..],
        let s' = battle (boost v s),
        null (infection s')]

-- increase hitpoints of all units in the immune system by v
boost :: Int -> State -> State
boost v s = s { immuneSystem = fmap boostDamage (immuneSystem s) }
  where
    boostDamage (Group n ut) =
        Group n ut { attackDamage = attackDamage ut + v }

tests2 :: [(String, Int)]
tests2 = [(testInput, 51)]

main :: IO ()
main = do
    s <- readFile "input/24.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
