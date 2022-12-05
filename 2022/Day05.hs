module Main where

import Utilities
import Parser
import Data.List

-- Input processing

type Input = (Configuration, [Move])
type Configuration = [Stack]
type Stack = [Crate]
type Crate = Char
data Move = Move Int Int Int
    deriving (Show)

parse :: String -> Input
parse s = (conf, moves)
  where
    conf = map (dropWhile (== ' ')) $ transpose $ map row $ init $ lines conf_s
    row = map head . takes 4 . tail
    [conf_s, moves_s] = paragraphs s
    moves = map (runParser move) (lines moves_s)
    move =
        Move <$ string "move " <*> nat <* string " from " <*>
            nat <* string " to " <*> nat

-- Part One

-- display a configuration (not needed, but useful for tracing)
showConfiguration :: Configuration -> String
showConfiguration stacks =
    unlines (map unwords (transpose (map formatStack stacks))) ++
        unwords footers ++ "\n"
  where
    formatStack cs = replicate (height - length cs) blank ++ map wrap cs
    wrap c = ['[', c, ']']
    blank = "   "
    height = maximum (map length stacks)
    footers = [column n | n <- [1..length stacks]]
    column n
      | n < 10 = " " ++ show n ++ " "
      | n < 100 = show n ++ " "
      | otherwise = show n

-- operate on one of the stacks

type Rest = ([Stack], [Stack])

get_stack :: Int -> Configuration -> (Stack, Rest)
get_stack pos stacks = (stack, (front, back))
  where
    (front, stack:back) = splitAt (pos-1) stacks

put_stack :: Stack -> Rest -> Configuration
put_stack stack (front, back) = front ++ (stack:back)

-- operate on a pile of crates

get_crates :: Int -> Int -> Configuration -> ([Crate], Configuration)
get_crates n src stacks = (take n stack, put_stack (drop n stack) rest)
  where
    (stack, rest) = get_stack src stacks

put_crates :: [Crate] -> Int -> Configuration -> Configuration
put_crates crates dest stacks = put_stack (crates ++ stack) rest
  where
    (stack, rest) = get_stack dest stacks

-- move crates one by one
action1 :: Configuration -> Move -> Configuration
action1 conf (Move n src dest) = put_crates (reverse crates) dest conf'
  where
    (crates, conf') = get_crates n src conf

solve1 :: Input -> String
solve1 (conf, moves) = map head $ foldl action1 conf moves

testInput :: String
testInput = "\
    \    [D]    \n\
    \[N] [C]    \n\
    \[Z] [M] [P]\n\
    \ 1   2   3 \n\
    \\n\
    \move 1 from 2 to 1\n\
    \move 3 from 1 to 3\n\
    \move 2 from 2 to 1\n\
    \move 1 from 1 to 2\n"

tests1 :: [(String, String)]
tests1 = [(testInput, "CMZ")]

-- Part Two

-- move crates together
action2 :: Configuration -> Move -> Configuration
action2 conf (Move n src dest) = put_crates crates dest conf'
  where
    (crates, conf') = get_crates n src conf

solve2 :: Input -> String
solve2 (conf, moves) = map head $ foldl action2 conf moves

tests2 :: [(String, String)]
tests2 = [(testInput, "MCD")]

main :: IO ()
main = do
    s <- readFile "input/05.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    putStrLn (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    putStrLn (solve2 input)
