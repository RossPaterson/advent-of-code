module Main where

import Parser
import Utilities
import Control.Applicative
import Data.List as List
import Data.Maybe
import Data.Map as Map hiding (map, foldl)

type Value = Int
type Bot = Int
type Bin = Int
data Instruction
    = Input Value Output
    | Distribute Bot Output Output
  deriving Show
data Output = Bot Bot | Output Bin
  deriving Show
type Input = [Instruction]

parse :: String -> Input
parse = map (runParser instruction) . lines
  where
    instruction = initInstr <|> distInstr
    initInstr = Input <$ string "value " <*> nat <* string " goes to " <*> output
    distInstr = Distribute <$ string "bot " <*> nat <* string " gives low to " <*> output <* string " and high to " <*> output
    output =
        Bot <$ string "bot " <*> nat <|>
        Output <$ string "output " <*> nat

data Action = Action {
    subject :: Bot,
    lvalue :: Value,
    ltarget :: Output,
    rvalue :: Value,
    rtarget :: Output }
  deriving Show

-- bot!i is a list of bot i's values in ascending order
type Bins = Map Int [Value]
data State = State { bot :: Bins, out :: Bins, history :: [Action] }
  deriving Show

initState :: State
initState = State Map.empty Map.empty []

startState :: Input -> State
startState instrs = foldl (flip id) initState [send v o | Input v o <- instrs]

send :: Value -> Output -> State -> State
send v (Bot i) (State bot out actions) = State (add i v bot) out actions
send v (Output i) (State bot out actions) = State bot (add i v out) actions

add :: Int -> Value -> Bins -> Bins
add i v m
  | member i m = adjust (List.insert v) i m
  | otherwise = Map.insert i [v] m

finalState :: Input -> State
finalState instrs = whileJust (distribute instrs) (startState instrs)

distribute :: Input -> State -> Maybe State
distribute instrs (State bot out actions) =
    listToMaybe [distr b ol oh (State bot out actions) |
        (b, vs) <- assocs bot, length vs >= 2,
        Distribute b' ol oh <- instrs, b' == b]

distr :: Bot -> Output -> Output -> State -> State
distr b ol oh (State bot out actions) =
    send low ol (send high oh (State bot' out actions'))
  where
    bot_vals = bot!b
    low = head bot_vals
    high = last bot_vals
    bot' = adjust (init . tail) b bot
    actions' = Action b low ol high oh:actions

solve1 :: Input -> Int
solve1 instrs = head [b | Action b 17 _ 61 _ <- history (finalState instrs)]

test = "value 5 goes to bot 2\nbot 2 gives low to bot 1 and high to bot 0\nvalue 3 goes to bot 1\nbot 1 gives low to output 1 and high to bot 0\nbot 0 gives low to output 2 and high to output 0\nvalue 2 goes to bot 2\n"

-- Part Two --

solve2 :: Input -> Int
solve2 instrs = product [head (output!i) | i <- [0,1,2]]
  where
    output = out (finalState instrs)

main :: IO ()
main = do
    s <- readFile "input/10.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
