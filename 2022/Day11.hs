module Main where

import Utilities
import Parser
import Control.Applicative
import Data.List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map

-- Input processing

type Input = Map MonkeyID Monkey
type MonkeyID = Int
data Monkey = Monkey {
    startingItems :: [WorryLevel],
    operation :: Operation,
    factor :: Int,
    thenTarget :: MonkeyID,
    elseTarget :: MonkeyID }
    deriving (Show)
type WorryLevel = Int
data Operation = Operation Operator Value
    deriving (Show)
data Operator = Plus | Times
    deriving (Show)
data Value = Constant Int | Old
    deriving (Show)

parse :: String -> Input
parse = Map.fromList . map (runParser monkey) . paragraphs
  where
    monkey = (,) <$ string "Monkey " <*> nat <* string ":\n" <*> code
    code = Monkey <$> start <*> op <*> test <*> then_part <*> else_part
    start =
        string "  Starting items: " *> sepBy1 nat (string ", ") <* char '\n'
    op =
        Operation <$ string "  Operation: new = old " <*> operator <*
        space <*> value <* char '\n'
    operator = Plus <$ char '+' <|> Times <$ char '*'
    value = Constant <$> nat <|> Old <$ string "old"
    test = string "  Test: divisible by " *> nat <* char '\n'
    then_part = string "    If true: throw to monkey " *> nat <* char '\n'
    else_part = string "    If false: throw to monkey " *> nat <* char '\n'

-- Part One

type Distribution = Map MonkeyID [WorryLevel]

initDistribution :: Map MonkeyID Monkey -> Distribution
initDistribution = Map.map startingItems

-- Inspection by a monkey of the items it is holding,
-- The description says the items are inspected in order and thrown ones
-- added to the back of the list, but actually the order doesn't matter.
-- To avoid numeric blowup, we use modulo arithmetic with a modulus chosen
-- so that all the decisions give the same results.
inspection :: Int -> Int -> Distribution -> MonkeyID -> Monkey -> Distribution
inspection relief_factor modulus d n monkey =
    foldr inspect (Map.insert n [] d) (d!n)
  where
    inspect item = Map.adjust (worry_level:) (target monkey worry_level)
      where
        worry_level =
            (apply (operation monkey) item `div` relief_factor) `mod` modulus

apply :: Operation -> Int -> Int
apply (Operation op arg) old = op_fn old arg_value
  where
    arg_value = case arg of
        Constant n -> n
        Old -> old
    op_fn = case op of
        Plus -> (+)
        Times -> (*)

-- where the new value gets thrown to
target :: Monkey -> WorryLevel -> MonkeyID
target monkey item
  | item `mod` factor monkey == 0 = thenTarget monkey
  | otherwise = elseTarget monkey

-- A round of inspections by all the monkeys
inspection_round :: Int -> Int -> Map MonkeyID Monkey -> Distribution ->
        Maybe (Map MonkeyID Int, Distribution)
inspection_round relief_factor modulus monkeys d0 =
    Just (Map.fromList activity, d')
  where
    (d', activity) = mapAccumL inspect_and_count d0 (Map.assocs monkeys)
    inspect_and_count d (n, monkey) =
        (inspection relief_factor modulus d n monkey, (n, length (d!n)))

-- Number of items inspected by each monkey on each round.
-- We use a modulus divisible by each factor, and also by the relief factor.
inspections :: Int -> Map MonkeyID Monkey -> [Map MonkeyID Int]
inspections relief_factor monkeys =
    unfoldr (inspection_round relief_factor modulus monkeys)
        (initDistribution monkeys)
  where
    modulus = foldr lcm relief_factor (map factor (Map.elems monkeys))

monkey_business :: Int -> Int -> Map MonkeyID Monkey -> Int
monkey_business nrounds relief_factor monkeys =
    product $ take 2 $ reverse $ sort $ Map.elems $ Map.unionsWith (+) $
        take nrounds $ inspections relief_factor monkeys

solve1 :: Input -> Int
solve1 = monkey_business 20 3

testInput :: String
testInput = "\
    \Monkey 0:\n\
    \  Starting items: 79, 98\n\
    \  Operation: new = old * 19\n\
    \  Test: divisible by 23\n\
    \    If true: throw to monkey 2\n\
    \    If false: throw to monkey 3\n\
    \\n\
    \Monkey 1:\n\
    \  Starting items: 54, 65, 75, 74\n\
    \  Operation: new = old + 6\n\
    \  Test: divisible by 19\n\
    \    If true: throw to monkey 2\n\
    \    If false: throw to monkey 0\n\
    \\n\
    \Monkey 2:\n\
    \  Starting items: 79, 60, 97\n\
    \  Operation: new = old * old\n\
    \  Test: divisible by 13\n\
    \    If true: throw to monkey 1\n\
    \    If false: throw to monkey 3\n\
    \\n\
    \Monkey 3:\n\
    \  Starting items: 74\n\
    \  Operation: new = old + 3\n\
    \  Test: divisible by 17\n\
    \    If true: throw to monkey 0\n\
    \    If false: throw to monkey 1\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 10605)]

-- Part Two

solve2 :: Input -> Int
solve2 = monkey_business 10000 1

tests2 :: [(String, Int)]
tests2 = [(testInput, 2713310158)]

main :: IO ()
main = do
    s <- readFile "input/11.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
