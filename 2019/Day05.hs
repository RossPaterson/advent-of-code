module Main where

import Intcode

-- Input processing

type Input = Memory

parse :: String -> Input
parse = readMemory

-- Part One

solve1 :: Input -> Value
solve1 mem
  | null vs = error "no output"
  | all (== 0) (init vs) = last vs
  | otherwise = error $ "diagnostics failed: " ++ show (init vs)
  where
    vs = streamFunction mem [1]

-- program and function it should implement
type Test = (String, Value -> Value)

-- test on a range of inputs and report failures
runTests :: [Test] -> [Value] -> IO ()
runTests tests vs = putStr $ unlines $
    [str ++ ": fails on " ++ show errs |
        (str, f) <- tests,
        let mem = readMemory str,
        let errs = testProgram mem f vs,
        not (null errs)]

-- Run the program with several inputs and return those for which it
-- produces different outputs than the function.
testProgram :: Memory -> (Value -> Value) -> [Value] -> [Value]
testProgram mem f xs = [x | x <- xs, streamFunction mem [x] /= [f x]]

tests1 :: [Test]
tests1 = [("3,0,4,0,99", id)]

-- Part Two

solve2 :: Input -> Value
solve2 mem
  | null vs = error "no output"
  | all (== 0) (init vs) = last vs
  | otherwise = error $ "diagnostics failed: " ++ show (init vs)
  where
    vs = streamFunction mem [5]

tests2 :: [Test]
tests2 = [
    ("3,9,8,9,10,9,4,9,99,-1,8", toValue . (== 8)),
    ("3,9,7,9,10,9,4,9,99,-1,8", toValue . (< 8)),
    ("3,3,1108,-1,8,3,4,3,99", toValue . (== 8)),
    ("3,3,1107,-1,8,3,4,3,99", toValue . (< 8)),
    ("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9", toValue . (/= 0)),
    ("3,3,1105,-1,9,1101,0,0,12,4,12,99,1", toValue . (/= 0)),
    ("3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,\
     \1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,\
     \999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99",
        \ n -> if n < 8 then 999 else if n == 8 then 1000 else 1001)]

main :: IO ()
main = do
    s <- readFile "input/05.txt"
    let input = parse s
    runTests tests1 [-5..10]
    print (solve1 input)
    runTests tests2 [-5..10]
    print (solve2 input)
