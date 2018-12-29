-- Turing machine
module Main where

import Parser
import Utilities
import Prelude hiding (Left, Right)
import Control.Applicative
import Data.Char
import Data.Map (Map, (!))
import qualified Data.Map as Map

type State = Char
type Value = Bool
data Direction = Left | Right
    deriving Show
data Transition = Transition Value Direction State
    deriving Show
type Machine = Map State (Transition, Transition)

data Input = Input { startState :: State, stopAfter :: Int, tm :: Machine }
    deriving Show

parse :: String -> Input
parse s = Input start steps machine
  where
    (s_intro:s_states) = map unlines $ paragraphs $ lines s
    (start, steps) = runParser intro s_intro
    intro = (,) <$
        string "Begin in state " <*> state <* string ".\n" <*
        string "Perform a diagnostic checksum after " <*> nat <* string " steps.\n"
    machine = Map.fromList (map (runParser transitions) s_states)
    transitions =
        (,) <$ string "In state " <*> state <* string ":\n" <*>
            ((,) <$> transition <*> transition)
    transition = Transition <$
        string "  If the current value is " <* value <* string ":\n" <*
        string "    - Write the value " <*> value <* string ".\n" <*
        string "    - Move one slot to the " <*> direction <* string ".\n" <*
        string "    - Continue with state " <*> state <* string ".\n"
    state = satisfy isUpper
    direction = Left <$ string "left" <|> Right <$ string "right"
    value = False <$ char '0' <|> True <$ char '1'

paragraphs :: [[a]] -> [[[a]]]
paragraphs ls = para:case rest of
    [] -> []
    _:ls' -> paragraphs ls'
  where
    (para, rest) = span (not . null) ls

-- infinite tape padded with False
data Tape =
    Tape { leftValues :: ![Value], cursor :: !Value, rightValues :: ![Value] }
    deriving Show

emptyTape :: Tape
emptyTape = Tape { leftValues = [], cursor = False, rightValues = [] }

-- move the cursor left or right
move :: Direction -> Tape -> Tape
move Left s = Tape {
    leftValues = l, cursor = sym,
    rightValues = pushValue (cursor s) (rightValues s) }
  where
    (sym, l) = popValue (leftValues s)
move Right s = Tape {
    leftValues = pushValue (cursor s) (leftValues s),
    cursor = sym, rightValues = r }
  where
    (sym, r) = popValue (rightValues s)

popValue :: [Bool] -> (Bool, [Bool])
popValue [] = (False, [])
popValue (b:bs) = (b, bs)

pushValue :: Bool -> [Bool] -> [Bool]
pushValue False [] = [] -- trim
pushValue b bs = b:bs

-- number of ones on the tape
checksum :: Tape -> Int
checksum s = ones (leftValues s) + ones (cursor s : rightValues s)
  where
    ones = length . filter id

-- state of the Turing machine
data TMState = TMState { current :: !State, tape :: !Tape }
    deriving Show

initState :: State -> TMState
initState s = TMState { current = s, tape = emptyTape }

-- one step of the Turing machine
step :: Machine -> TMState -> TMState
step mc s = apply (selectPair (cursor (tape s)) (mc!current s)) s

selectPair :: Bool -> (a, a) -> a
selectPair False (f, t) = f
selectPair True (f, t) = t

apply :: Transition -> TMState -> TMState
apply (Transition value dir newState) s = TMState {
    current = newState,
    tape = move dir $ (tape s) { cursor = value } }

solve1 :: Input -> Int
solve1 input = checksum $ tape $
    times (stopAfter input) (step (tm input)) (initState (startState input))

testInput :: String
testInput = "\
    \Begin in state A.\n\
    \Perform a diagnostic checksum after 6 steps.\n\
    \\n\
    \In state A:\n\
    \  If the current value is 0:\n\
    \    - Write the value 1.\n\
    \    - Move one slot to the right.\n\
    \    - Continue with state B.\n\
    \  If the current value is 1:\n\
    \    - Write the value 0.\n\
    \    - Move one slot to the left.\n\
    \    - Continue with state B.\n\
    \\n\
    \In state B:\n\
    \  If the current value is 0:\n\
    \    - Write the value 1.\n\
    \    - Move one slot to the left.\n\
    \    - Continue with state A.\n\
    \  If the current value is 1:\n\
    \    - Write the value 1.\n\
    \    - Move one slot to the right.\n\
    \    - Continue with state A.\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 3)]

tests2 :: [(String, Int)]
tests2 = [(testInput, 9)]

main :: IO ()
main = do
    s <- readFile "input/25.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
