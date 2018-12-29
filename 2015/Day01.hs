module Main where

solve1 :: String -> Int
solve1 = sum . map move

move :: Char -> Int
move '(' = 1
move ')' = -1
move _ = 0

tests = ["(())", "()()", "(((", "(()(()(", "))(((((", "())", "))(", ")))", ")())())"]

-- Part Two --

solve2 :: String -> Int
solve2 = length . takeWhile (/= -1) . scanl (+) 0 . map move

tests2 = [")", "()())"]

main :: IO ()
main = do
    s <- readFile "input/01.txt"
    print (solve1 s)
    print (solve2 s)
