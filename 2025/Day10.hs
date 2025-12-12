module Main where

import Parser
import Utilities
import Control.Applicative (some, (<|>))
import Data.List (subsequences, unfoldr)
import Data.Maybe (mapMaybe)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.CompactSet (Set)
import qualified Data.CompactSet as Set

-- Input processing

type Input = [Machine]
type Machine = (Lights, [Button], Map SlotNo Int)
type Lights = Set SlotNo
type Button = Set SlotNo -- slots affected by the button
type SlotNo = Int -- position of indicator light or joltage level

parse :: String -> Input
parse = map (runParser machine) . lines
  where
    machine = (,,) <$> lights <* char ' ' <*> buttons <* char ' ' <*> joltage
    lights = setLights <$ char '[' <*> some light <* char ']'
    light = False <$ char '.' <|> True <$ char '#'
    buttons = button `sepBy1` char ' '
    button = Set.fromList <$ char '(' <*> numbers <* char ')'
    joltage = (Map.fromList . zip [0..]) <$ char '{' <*> numbers <* char '}'
    numbers = nat `sepBy1` char ','

setLights :: [Bool] -> Lights
setLights bs = Set.fromList [n | (n, b) <- zip [0..] bs, b]

-- Part One

solve1 :: Input -> Int
solve1 = sum . map fewestPresses

-- Since pressing a button a second time undoes its effect, we need only
-- consider pressing or not pressing each button.
fewestPresses :: Machine -> Int
fewestPresses (target, bs, _) =
    minimum $ map length $
    filter (\ sub_bs -> foldr xorSet Set.empty sub_bs == target) $
    subsequences bs

xorSet :: Set a -> Set a -> Set a
xorSet a b = Set.difference (Set.union a b) (Set.intersection a b)

testInput :: String
testInput = "\
    \[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}\n\
    \[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}\n\
    \[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 7)]

-- Part Two

solve2 :: Input -> Int
solve2 = sum . map (minimalSolution . linearSystem)

minimalSolution :: LinearSystem -> Int
minimalSolution = minimum . map sum . solveLinearSystem

-- a system of linear equations
type LinearSystem = [Constraint]

-- a constraint in the form of a linear equation
-- invariant: the map is non-empty and all coefficients are non-zero
type Constraint = (Int, Map Variable Int)

-- a variable in this system corresponds to the number of presses of a
-- particular button in the original problem
type Variable = Char

variables :: [Variable]
variables = ['A'..'Z']

-- The initial linear system has a constraint on button presses for each
-- slot, with all coefficients either 1 (if the button contributes the
-- a slot) or 0.
linearSystem :: Machine -> LinearSystem
linearSystem (_, bs, levels) =
    [(t, Map.fromList [(v, 1) | (v, b) <- zip variables bs, Set.member n b]) |
        (n, t) <- Map.assocs levels]

-- All possible non-negative solutions of the linear system
solveLinearSystem :: LinearSystem -> [Map Variable Int]
solveLinearSystem cs =
    solveConstraints (upperBound cs) (reverse (upperTriangular cs)) Map.empty

-- maximum possible value for each variable
upperBound :: LinearSystem -> Map Variable Int
upperBound cs =
    Map.fromListWith min [(v, t) | (t, vs) <- cs, v <- Map.keys vs]

-- Convert the linear system to an equivalent upper triangular form
-- using an integer version of Gaussian elimination.
upperTriangular :: LinearSystem -> LinearSystem
upperTriangular = unfoldr elimVariable

-- Eliminate a variable constrained by the first equation from all
-- following constraints, removing any constraints that become trivial.
elimVariable :: LinearSystem -> Maybe (Constraint, LinearSystem)
elimVariable [] = Nothing
elimVariable (c:cs) = Just (c, mapMaybe (eliminate c) cs)

-- Form a linear combination of the two constraints that has a zero
-- coefficient for a selected variable (the pivot) that is constrained
-- by the first equation.
eliminate :: Constraint -> Constraint -> Maybe Constraint
eliminate p@(_, pm) c@(_, cm)
  | cv == 0 = Just c
  | otherwise =
    addConstraint
        (scaleConstraint (pv `div` f) c)
        (scaleConstraint (- cv `div` f) p)
  where
    -- choose any variable with a non-zero coefficient as the pivot
    (v, pv) = Map.findMin pm
    cv = Map.findWithDefault 0 v cm
    f = gcd pv cv

scaleConstraint :: Int -> Constraint -> Constraint
scaleConstraint n (t, m) = (n*t, Map.map (n*) m)

addConstraint :: Constraint -> Constraint -> Maybe Constraint
addConstraint (t1, m1) (t2, m2)
  | Map.null m = Nothing
  | otherwise = Just (t1+t2, m)
  where
    m = Map.filter (/= 0) (Map.unionWith (+) m1 m2)

-- Given a linear system in lower triangular form, find possible values
-- for the variables it contains.
solveConstraints ::
    Map Variable Int -> LinearSystem -> Map Variable Int -> [Map Variable Int]
solveConstraints _ [] m = [m]
solveConstraints bounds (c:cs) m =
    [m'' |
        m' <- solveConstraint bounds t (Map.assocs vs) m,
        m'' <- solveConstraints bounds cs m']
  where
    (t, vs) = substitute m c

-- Apply a partial substitution to a constraint
substitute :: Map Variable Int -> Constraint -> Constraint
substitute s (t, vs) =
    (t - sum (Map.intersectionWith (*) vs s), Map.difference vs s)

-- Generate possible values for each variable in the linear equation.
solveConstraint ::
    Map Variable Int -> Int -> [(Variable, Int)] ->
        Map Variable Int -> [Map Variable Int]
solveConstraint _ _ [] _ = error "unused solveConstraint case"
-- The last variable has at most one possible value.
solveConstraint _ t [(v, a)] m =
    [Map.insert v n m | t `mod` a == 0 && n >= 0]
  where
    n = t `div` a
-- Other variables range up to their bounds from the original system.
solveConstraint bounds t ((v, a):vns) m =
    [ m' |
        n <- [0..Map.findWithDefault 0 v bounds],
        m' <- solveConstraint bounds (t - n*a) vns (Map.insert v n m)]

tests2 :: [(String, Int)]
tests2 = [(testInput, 33)]

main :: IO ()
main = do
    s <- readFile "input/10.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
