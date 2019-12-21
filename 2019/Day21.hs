module Main where

import Intcode

-- Input processing

type Input = Memory

parse :: String -> Input
parse = readMemory

-- Part One

-- Jump if there's a safe landing that you can't reach by walking:
-- NOT (A AND B AND C) AND NOT D
springProg1 :: String
springProg1 =
    "OR A T\n\
    \AND B T\n\
    \AND C T\n\
    \NOT T J\n\
    \AND D J\n\
    \WALK\n"

solve1 :: Input -> Integer
solve1 mem = last $ streamFunction mem $ map toValue springProg1

-- Part Two

-- This works, but not sure if it's complete
-- NOT (A AND B AND C) AND NOT D AND (E OR H)
springProg2 :: String
springProg2 =
    "OR A T\n\
    \AND B T\n\
    \AND C T\n\
    \NOT T J\n\
    \AND D J\n\
    \NOT E T\n\
    \NOT T T\n\
    \OR H T\n\
    \AND T J\n\
    \RUN\n"

-- This also works
-- NOT (A AND B AND C) AND NOT D AND ((E AND (F OR I)) OR H)
springProg2a :: String
springProg2a =
    "OR F J\n\
    \OR I J\n\
    \AND E J\n\
    \OR H J\n\
    \OR A T\n\
    \AND B T\n\
    \AND C T\n\
    \NOT T T\n\
    \AND T J\n\
    \AND D J\n\
    \RUN\n"

solve2 :: Input -> Integer
solve2 mem = last $ streamFunction mem $ map toValue springProg2

main :: IO ()
main = do
    s <- readFile "input/21.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
