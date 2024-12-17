module Main where

import Parser
import Utilities
import Data.Bits
import Data.List
import Data.Maybe
import Data.Map (Map, (!))
import qualified Data.Map as Map

-- Input processing

type Input = Machine

type Machine = (Registers, Program)

type Registers = Map Register Int
data Register = A | B | C
    deriving (Bounded, Enum, Eq, Ord, Read, Show)
type Program = Map Int Int

parse :: String -> Machine
parse s = case paragraphs s of
    [registers_s, program_s] ->
        (Map.fromList (map (runParser register) (lines registers_s)),
         Map.fromList (zip [0..] (runParser program program_s)))
    _ -> error "bad input"
  where
    register = (,) <$ string "Register " <*> enumValue <* string ": " <*> int
    program = string "Program: " *> (nat `sepBy1` char ',') <* char '\n'

-- Part One

data State = State Registers Int
    deriving (Show)

initState :: Registers -> State
initState regs = State regs 0

-- machine instructions
data Instruction
    = ADV Operand
    | BXL Int
    | BST Operand
    | JNZ Int
    | BXC Int
    | OUT Operand
    | BDV Operand
    | CDV Operand
    deriving (Show)

decodeInstruction :: Int -> Int -> Instruction
decodeInstruction op arg = case op of
    0 -> ADV (decodeOperand arg)
    1 -> BXL arg
    2 -> BST (decodeOperand arg)
    3 -> JNZ arg
    4 -> BXC arg
    5 -> OUT (decodeOperand arg)
    6 -> BDV (decodeOperand arg)
    7 -> CDV (decodeOperand arg)
    _ -> error $ "illegal opcode " ++ show op

-- combo operands
data Operand = Literal Int | Reg Register | Reserved
    deriving (Show)

decodeOperand :: Int -> Operand
decodeOperand n
  | 0 <= n && n <= 3 = Literal n
  | 4 <= n && n <= 6 = Reg (toEnum (n-4))
  | n == 7 = Reserved
  | otherwise = error $ "illegal combo operand " ++ show n

-- one fetch-execute cycle
step :: Program -> State -> Maybe (Maybe Int, State)
step prog (State regs ip) = do
    (instr, ip') <- fetch prog ip
    let s' = State regs ip'
    return $ case execute instr s' of
        Left n -> (Just n, s')
        Right s'' -> (Nothing, s'')

fetch :: Program -> Int -> Maybe (Instruction, Int)
fetch prog ip
  | ip+1 >= Map.size prog = Nothing
  | otherwise = Just (instr, ip+2)
  where
    instr = decodeInstruction (prog!ip) (prog!(ip+1))

execute :: Instruction -> State -> Either Int State
execute (ADV arg) (State regs ip) =
    Right (State (Map.insert A (division regs arg) regs) ip)
execute (BXL n) (State regs ip) =
    Right (State (Map.insert B (regs!B `xor` n) regs) ip)
execute (BST arg) (State regs ip) =
    Right (State (Map.insert B (operandValue regs arg `mod` 8) regs) ip)
execute (JNZ n) (State regs ip) =
    Right (State regs (if regs!A == 0 then ip else n))
execute (BXC _) (State regs ip) =
    Right (State (Map.insert B (regs!B `xor` regs!C) regs) ip)
execute (OUT arg) (State regs _) =
    Left (operandValue regs arg `mod` 8)
execute (BDV arg) (State regs ip) =
    Right (State (Map.insert B (division regs arg) regs) ip)
execute (CDV arg) (State regs ip) =
    Right (State (Map.insert C (division regs arg) regs) ip)

-- division by a power of 2 (right shift)
division :: Registers -> Operand -> Int
division regs arg = shiftR (regs!A) (operandValue regs arg)

operandValue :: Registers -> Operand -> Int
operandValue _ (Literal n) = n
operandValue regs (Reg r) = regs!r
operandValue _ Reserved = error "reserved"

-- run the program and collect its output
runProgram :: Program -> Registers -> [Int]
runProgram prog = catMaybes . unfoldr (step prog) . initState

solve1 :: Input -> String
solve1 (regs, prog) = intercalate "," $ map show $ runProgram prog regs

-- Tracing the program execution

trace :: Program -> State -> [(Int, Instruction, Registers)]
trace prog = unfoldr (traceStep prog)

traceStep :: Program -> State -> Maybe ((Int, Instruction, Registers), State)
traceStep prog (State regs ip) = do
    (instr, ip') <- fetch prog ip
    let s' = State regs ip'
    return $ case execute instr s' of
        Left _ -> ((ip, instr, regs), s')
        Right s''@(State regs' _) -> ((ip, instr, regs'), s'')

-- Debug output

showTrace :: Program -> State -> String
showTrace prog s@(State regs _) =
    unlines (showRegisters regs : map showStep (trace prog s))

showStep :: (Int, Instruction, Registers) -> String
showStep (n, instr, regs) =
    show n ++ ": " ++ showInstruction instr ++ "\n" ++ showRegisters regs

showInstruction :: Instruction -> String
showInstruction (ADV arg) =
    "adv " ++ showOperand arg ++ "\tA = A >> " ++ showOperand arg
showInstruction (BXL n) =
    "bxl " ++ show n ++ "\tB = B ^ " ++ show n
showInstruction (BST arg) =
    "bst " ++ showOperand arg ++ "\tB = " ++ showOperand arg ++ " % 8"
showInstruction (JNZ n) =
    "jnz " ++ show n ++ "\tif (A != 0) goto " ++ show n
showInstruction (BXC n) =
    "bxc " ++ show n ++ "\tB = B ^ C"
showInstruction (OUT arg) =
    "out " ++ showOperand arg ++ "\tprint B%8"
showInstruction (BDV arg) =
    "bdv " ++ showOperand arg ++ "\tB = A >> " ++ showOperand arg
showInstruction (CDV arg) =
    "cdv " ++ showOperand arg ++ "\tC = A >> " ++ showOperand arg

showOperand :: Operand -> String
showOperand (Literal n) = show n
showOperand (Reg r) = show r
showOperand Reserved = "*"

showRegisters :: Registers -> String
showRegisters regs =
    unlines ["Register " ++ show r ++ ": " ++ show v |
        (r, v) <- Map.assocs regs]

testInput :: String
testInput = "\
    \Register A: 729\n\
    \Register B: 0\n\
    \Register C: 0\n\
    \\n\
    \Program: 0,1,5,4,3,0\n\
    \"

tests1 :: [(String, String)]
tests1 = [(testInput, "4,6,3,5,6,3,5,2,1,0")]

-- Part Two

{-
The input program has the form

	... operations setting B and C from A ...
	out B
	adv 3
	jnz 0
i.e.
	do {
		... operations setting B and C from A ...
		print B%8;
		A /= 8;
	} while (A /= 0);

That is, it consists of a single loop, with each iteration outputing
a single value and then shifting A right by 3.  Only the value of A is
used in each iteration.
-}

-- Run an iteration of the loop backwards: given the output v and the
-- final value of A, return all the values of A at the start of the loop
-- that would produce these.
back :: Program -> Int -> Int -> [Int]
back prog v a_end =
    [a_start |
        d <- [0..7],
        let a_start = a_end*8 + d,
        head (runProgram prog (Map.singleton A a_start)) == v]

backs :: Program -> Int -> [Int] -> [Int]
backs prog v = concatMap (back prog v)

-- Given output list, run the machine backwards from a final state
-- with A == 0 to all possible initial values of A that would produce
-- that output.
runBackwards :: Program -> [Int] -> [Int]
runBackwards prog = foldr (backs prog) [0]

solve2 :: Input -> Int
solve2 (_, prog) = minimum $ runBackwards prog $ Map.elems prog

main :: IO ()
main = do
    s <- readFile "input/17.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    putStrLn (solve1 input)
    print (solve2 input)
