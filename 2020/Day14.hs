module Main where

import Utilities
import Parser
import Control.Applicative
import Data.Bits
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

-- Input processing

type Input = [Command]

data Command = BitMask BitMask | Write Int Int
    deriving Show
type BitMask = [Maybe Bool]

parse :: String -> Input
parse = map (runParser command) . lines
  where
    command =
        BitMask <$ string "mask = " <*> bitmask <|>
        Write <$ string "mem[" <*> nat <* string "] = " <*> nat
    bitmask = reverse <$> some bit_or_X
    bit_or_X = Just <$> onebit <|> Nothing <$ char 'X'
    onebit = False <$ char '0' <|> True <$ char '1'

-- Part One

-- summary data from a mask
-- This version generalizes from fixed-size masks by assuming leading zeros.
data Mask = Mask {
    ones_mask :: Int, -- mask of positions containing a 1
    nonzero_mask :: Int, -- mask of positions containing 1 or X
    wild_indices :: [Int] -- list of positions containing an X
    }
    deriving Show

readMask :: [Maybe Bool] -> Mask
readMask mbs = Mask {
    ones_mask = bits one_indices,
    nonzero_mask = bits (one_indices ++ x_indices),
    wild_indices = x_indices }
  where
    one_indices = indices (Just True) mbs
    x_indices = indices Nothing mbs

-- value with bits at the listed positions set
bits :: [Int] -> Int
bits = foldr (.|.) 0 . map bit

-- positions (counting from 0) that x occurs in ys
indices :: Eq a => a -> [a] -> [Int]
indices x ys = [i | (i, y) <- zip [0..] ys, y == x]

data State = State { mask :: Mask, memory :: Map Int Int }
    deriving Show

initState :: State
initState = State { mask = error "undefined mask", memory = Map.empty }

-- In part one, the mask modifies values assigned.

action1 :: State -> Command -> State
action1 s (BitMask mbs) = s { mask = readMask mbs }
action1 s (Write loc val) =
    s { memory = Map.insert loc (applyMask1 (mask s) val) (memory s) }

applyMask1 :: Mask -> Int -> Int
applyMask1 m val = ones_mask m .|. (nonzero_mask m .&. val)

solve1 :: Input -> Int
solve1 = sum . Map.elems . memory . foldl action1 initState

testInput1 :: String
testInput1 = "\
    \mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\n\
    \mem[8] = 11\n\
    \mem[7] = 101\n\
    \mem[8] = 0\n"

tests1 :: [(String, Int)]
tests1 = [(testInput1, 165)]

-- Part Two

-- In part two, the mask modifies the locations assigned to.

action2 :: State -> Command -> State
action2 s (BitMask mbs) = s { mask = readMask mbs }
action2 s (Write loc val) = s { memory = Map.union assignments (memory s) }
  where
    assignments = Map.fromList [(loc', val) | loc' <- applyMask2 (mask s) loc]

applyMask2 :: Mask -> Int -> [Int]
applyMask2 m loc =
    map (xor (ones_mask m .|. loc) . bits) $ subsequences $ wild_indices m

solve2 :: Input -> Int
solve2 = sum . Map.elems . memory . foldl action2 initState

testInput2 :: String
testInput2 = "\
    \mask = 000000000000000000000000000000X1001X\n\
    \mem[42] = 100\n\
    \mask = 00000000000000000000000000000000X0XX\n\
    \mem[26] = 1\n"

tests2 :: [(String, Int)]
tests2 = [(testInput2, 208)]

main :: IO ()
main = do
    s <- readFile "input/14.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
