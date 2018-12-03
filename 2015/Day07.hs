{-# LANGUAGE DeriveFunctor #-}
module Main where

import Parser
import Control.Applicative
import Data.Bits
import Data.Char
import Data.Word
import Data.Map (Map, (!))
import qualified Data.Map as Map

data Value a = Literal Word16 | Wire a
  deriving (Show, Functor)

data Gate a
    = COPY (Value a)
    | AND (Value a) (Value a)
    | OR (Value a) (Value a)
    | LSHIFT (Value a) Int
    | RSHIFT (Value a) Int
    | NOT (Value a)
  deriving (Show, Functor)

type Wire = String
type Instruction = (Wire, Gate Wire)
type Input = [Instruction]

parse :: String -> Input
parse = map (runParser instruction) . lines
  where
    instruction = flip (,) <$> gate <* string " -> " <*> wire
    gate =
        COPY <$> value <|>
        AND <$> value <* string " AND " <*> value <|>
        OR <$> value <* string " OR " <*> value <|>
        LSHIFT <$> value <* string " LSHIFT " <*> nat <|>
        RSHIFT <$> value <* string " RSHIFT " <*> nat <|>
        NOT <$ string "NOT " <*> value
    value = (Literal . fromIntegral) <$> nat <|> Wire <$> wire
    wire = some (satisfy isLower)

evalCircuit :: Input -> Map Wire Word16 -> Map Wire Word16
evalCircuit instrs values =
    Map.fromList [(w, eval (fmap (values!) g)) | (w, g) <- instrs]

eval :: Gate Word16 -> Word16
eval (COPY i) = evalValue i
eval (AND x y) = evalValue x .&. evalValue y
eval (OR x y) = evalValue x .|. evalValue y
eval (LSHIFT x k) = shift (evalValue x) k
eval (RSHIFT x k) = shift (evalValue x) (-k)
eval (NOT x) = complement (evalValue x)

evalValue :: Value Word16 -> Word16
evalValue (Literal i) = i
evalValue (Wire v) = v

solve1 :: Input -> Word16
solve1 instrs = values!"a"
  where -- memoize the value map
    values = evalCircuit instrs values

test =
    "123 -> x\n\
    \456 -> y\n\
    \x AND y -> d\n\
    \x OR y -> e\n\
    \x LSHIFT 2 -> f\n\
    \y RSHIFT 2 -> g\n\
    \NOT x -> h\n\
    \NOT y -> i\n"

-- Part Two --

solve2 :: Input -> Word16
solve2 instrs = values!"a"
  where
    values = Map.insert "b" (solve1 instrs) (evalCircuit instrs values)

main :: IO ()
main = do
    s <- readFile "input07.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
