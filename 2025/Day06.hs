module Main where

import Utilities
import Data.List (transpose)

-- Input processing

type Input = ([[Int]], [Op])

data Op = Plus | Times
    deriving (Show)

parse :: String -> Input
parse s = (map (map read) (init wss), map toOp (last wss))
  where
    wss = map words (lines s)

toOp :: String -> Op
toOp "+" = Plus
toOp "*" = Times
toOp s = error $ "bad op " ++ s

-- Part One

solve1 :: Input -> Int
solve1 (nss, ops) = sum $ map evaluate $ zip (transpose nss) ops

evaluate :: ([Int], Op) -> Int
evaluate (ns, Plus) = sum ns
evaluate (ns, Times) = product ns

testInput :: String
testInput = "\
    \123 328  51 64 \n\
    \ 45 64  387 23 \n\
    \  6 98  215 314\n\
    \*   +   *   +  \n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 4277556)]

-- Part Two

type Input2 = [([Int], Op)]

parse2 :: String -> Input2
parse2 s = map parseBlock $ splitOn spaces $ transpose ls
  where
    ls = lines s
    spaces = map (const ' ') ls

-- parse one transposed block
parseBlock :: [String] -> ([Int], Op)
parseBlock block =
    (map read (map init block), toOp (take 1 (map last block)))

-- break the list into sublists by a separator
splitOn :: Eq a => a -> [a] -> [[a]]
splitOn sep xs = case span (/= sep) xs of
    (col, _:rest) -> col : splitOn sep rest
    _ -> [xs]

solve2 :: Input2 -> Int
solve2 = sum . map evaluate

tests2 :: [(String, Int)]
tests2 = [(testInput, 3263827)]

main :: IO ()
main = do
    s <- readFile "input/06.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    let input2 = parse2 s
    putStr (unlines (failures "solve2" (solve2 . parse2) tests2))
    print (solve2 input2)
