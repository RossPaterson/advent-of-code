module Main where

import Geometry
import Parser
import Utilities
import Control.Applicative
import Data.Char
import Data.Foldable
import Data.Tree
import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as Map

-- Input processing

type System = Map Label [Rule]
type Label = String
data Rule
    = Conditional Variable Comparison Int Outcome
    | Final Outcome
    deriving (Show)
data Outcome = Accept | Reject | Goto Label
    deriving (Eq, Ord, Show)
data Comparison = LessThan | GreaterThan
    deriving (Show)

type Part = Map Variable Int
type Variable = Char

type Input = (System, [Part])

parse :: String -> Input
parse s = case paragraphs s of
    [p1, p2] ->
        (Map.fromList (map (runParser workflow) (lines p1)),
         map (runParser part) (lines p2))
    _ -> error "not two paragraphs"
  where
    workflow = (,) <$> label <* char '{' <*> sepBy1 rule (char ',') <* char '}'
    rule =
        Conditional <$> variable <*> comp <*> nat <* char ':' <*> outcome <|>
        Final <$> outcome
    outcome =
        Accept <$ char 'A' <|>
        Reject <$ char 'R' <|>
        Goto <$> label
    label = (:) <$> satisfy isLower <*> some (satisfy isLower)
    comp = LessThan <$ char '<' <|> GreaterThan <$ char '>'
    part =
        Map.fromList <$ char '{' <*> sepBy1 assignment (char ',') <* char '}'
    assignment = (,) <$> variable <* char '=' <*> nat
    variable = satisfy (`elem` variables)

variables :: [Variable]
variables = "xmas"

-- Part One

start :: Outcome
start = Goto "in"

-- apply a workflow to a part
step :: System -> Part -> Outcome -> Outcome
step _ _ Accept = Accept
step _ _ Reject = Reject
step system part (Goto l) =
    foldr (applyRule part) (error "bad rules") (system!l)

applyRule :: Part -> Rule -> Outcome -> Outcome
applyRule _ (Final o) _ = o
applyRule part (Conditional v rel n o) o'
  | compareInt (part!v) rel n = o
  | otherwise = o'

compareInt :: Int -> Comparison -> Int -> Bool
compareInt x LessThan y = x < y
compareInt x GreaterThan y = x > y

-- Does applying the system to this part yield Accept?
accept :: System -> Part -> Bool
accept system part =
    any (== Accept) $ takeWhile (/= Reject) $
        iterate (step system part) start

totalRating :: Part -> Int
totalRating = sum . Map.elems

solve1 :: Input -> Int
solve1 (system, parts) =
    sum $ map totalRating $ filter (accept system) parts

testInput :: String
testInput = "\
    \px{a<2006:qkq,m>2090:A,rfg}\n\
    \pv{a>1716:R,A}\n\
    \lnx{m>1548:A,A}\n\
    \rfg{s<537:gd,x>2440:R,A}\n\
    \qs{s>3448:A,lnx}\n\
    \qkq{x<1416:A,crn}\n\
    \crn{x>2662:A,R}\n\
    \in{s<1351:px,qqz}\n\
    \qqz{s>2770:qs,m<1801:hdj,R}\n\
    \gd{a>3333:R,R}\n\
    \hdj{m>838:A,pv}\n\
    \\n\
    \{x=787,m=2655,a=1222,s=2876}\n\
    \{x=1679,m=44,a=2067,s=496}\n\
    \{x=2036,m=264,a=79,s=2244}\n\
    \{x=2461,m=1339,a=466,s=291}\n\
    \{x=2127,m=1623,a=2188,s=1013}\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 19114)]

-- Part Two

minValue, maxValue :: Int
minValue = 1
maxValue = 4000

-- non-empty range of integers
type Range = AABox Int

-- non-empty range (lo <= hi)
range :: Int -> Int -> Range
range lo hi = boundingBox [lo, hi]

-- possible values for each variable
variableValues :: Range
variableValues = range minValue maxValue

-- a cartesian combination of ranges
type PartSet = Map Variable Range

numCombinations :: PartSet -> Int
numCombinations = product . map boxSize . Map.elems

-- the outcome for all combinations in the set
type State = (PartSet, Outcome)

-- initially each variable can take any possible value
startState :: State
startState = (Map.fromList [(v, variableValues) | v <- variables], start)

-- disjoint states obtained by applying one workflow to the given state
nextStates :: System -> State -> [State]
nextStates system (c, Goto l) = states c (system!l)
nextStates _ _ = []

-- disjoint states resulting from executing the rules on a given combination
states :: PartSet -> [Rule] -> [State]
states c [Final o] = [(c, o)]
states c (Conditional v rel n o:rs) =
    [(Map.insert v r' c, o) | r' <- maybeToList (intersectBox r cond)] ++
    [s | r' <- diffBox r cond, s <- states (Map.insert v r' c) rs]
  where
    r = c!v
    cond = successRange rel n
states _ _ = error "bad rules"

-- subrange on which the comparison succeeds
successRange :: Comparison -> Int -> Range
successRange LessThan y = range minValue (y-1)
successRange GreaterThan y = range (y+1) maxValue

-- tree of states reachable from the start state
-- The combination sets in each branch are disjoint subsets of the set
-- in the parent branch.
stateTree :: System -> Tree State
stateTree system = iterateTree (nextStates system) startState

-- disjoint sets of combinations that lead to Accept
acceptable :: System -> [PartSet]
acceptable system =
    map fst $ filter ((== Accept) . snd) $ toList $ stateTree system

solve2 :: Input -> Int
solve2 (system, _) = sum (map numCombinations (acceptable system))

tests2 :: [(String, Int)]
tests2 = [(testInput, 167409079868000)]

main :: IO ()
main = do
    s <- readFile "input/19.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
