module Main where

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
isMul (Mul r x) = True
isMul _ = False

solve1 :: Input -> Int
solve1 program = length $ filter (isMul . (program!)) $ init $
    map pc $ iterateWhileJust (step program) initState

-- Part Two

-- n has non-trivial factors
composite :: Int -> Bool
composite n = or [n `mod` f == 0 | f <- takeWhile ((<= n) . (^2)) [2..]]

getValue :: Instruction -> Int
getValue (Set r (Value x)) = x
getValue (Sub r (Value x)) = x
getValue (Mul r (Value x)) = x

solve2 :: Input -> Int
solve2 program = length $ filter composite [start, start+incr .. finish]
  where
    start = getValue (program!0) * getValue (program!4) - getValue (program!5)
    finish = start - getValue (program!7)
    incr = - getValue (program!30)

main :: IO ()
main = do
    s <- readFile "input23.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
