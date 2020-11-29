module Main where

import Number
import Parser
import Utilities
import Control.Applicative
import Data.Array
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map

type Reg = Char
data Source = Reg Reg | Value Int
    deriving Show
data Instruction
    = Set Reg Source
    | Sub Reg Source
    | Mul Reg Source
    | Jnz Source Int
    deriving Show

type Code = Array Int Instruction

code :: [Instruction] -> Code
code instrs = listArray (0, length instrs-1) instrs

type Input = Code

parse :: String -> Input
parse = code . map (runParser instruction) . lines
  where
    instruction =
        Set <$ string "set " <*> reg <* string " " <*> source <|>
        Sub <$ string "sub " <*> reg <* string " " <*> source <|>
        Mul <$ string "mul " <*> reg <* string " " <*> source <|>
        Jnz <$ string "jnz " <*> source <* string " " <*> int
    source = Reg <$> reg <|> Value <$> int
    reg = satisfy isLower

data State = State {
    pc :: Int,
    register :: Map Char Int }

initState :: State
initState = State { pc = 0, register = Map.empty }

get :: State -> Source -> Int
get s (Reg r) = Map.findWithDefault 0 r (register s)
get _ (Value v) = v

set :: State -> Reg -> Int -> State
set s r v = s { register = Map.insert r v (register s) }

advance :: State -> State
advance s = s { pc = pc s + 1 }

execute :: State -> Instruction -> State
execute s (Set r x) = advance (set s r (get s x))
execute s (Sub r x) = advance (set s r (get s (Reg r) - get s x))
execute s (Mul r x) = advance (set s r (get s (Reg r) * get s x))
execute s (Jnz x y)
  | get s x /= 0 = s { pc = pc s + y }
  | otherwise = advance s

step :: Code -> State -> Maybe State
step program s
  | inRange (bounds program) loc = Just (execute s (program!loc))
  | otherwise = Nothing
  where
    loc = pc s

isMul :: Instruction -> Bool
isMul (Mul _ _) = True
isMul _ = False

solve1 :: Input -> Int
solve1 program = length $ filter (isMul . (program!)) $ init $
    map pc $ iterateWhileJust (step program) initState

-- Part Two

{-
The outer structure of the program is:
 0: set b <v0>
    ...
 4: mul b <v4>
 5: sub b <v5>
 6: set c b
 7: sub c <v7>
    ... if b has factors, increment h ...
26: set g b
27: sub g c
28: jnz g +2
29: jump to address 32 (exit)
30: sub b <v30>
31: jump to address 8

That is:
    b = <v0>*<v4> - <v5>
    c = b - <v7>
    while (1) {
        if (b has factors)
            h++
        if (b == c)
            exit
        b = b - <v30>

All the subtracted values are negative, so they are additions.
-}

-- n has non-trivial factors
composite :: Int -> Bool
composite n = not (isPrime n)

getValue :: Instruction -> Int
getValue (Set _ (Value x)) = x
getValue (Sub _ (Value x)) = x
getValue (Mul _ (Value x)) = x
getValue instr = error $ "unexpected instruction " ++ show instr

solve2 :: Input -> Int
solve2 program = length $ filter composite [start, start+incr .. finish]
  where
    start = getValue (program!0) * getValue (program!4) - getValue (program!5)
    finish = start - getValue (program!7)
    incr = - getValue (program!30)

main :: IO ()
main = do
    s <- readFile "input/23.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
