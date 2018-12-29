module Main where

import Parser
import Control.Applicative
import Data.Char
import Numeric

-- Although we only need sizes, might as well construct the lists

unescape :: String -> String
unescape = runParser literal
  where
    literal = char '"' *> many oneChar <* char '"'
    oneChar = char '\\' *> escape <|> satisfy ordinary
    escape =
        hexChar <$ char 'x' <*> hexDigit <*> hexDigit <|>
        satisfy (/= 'x')
    hexChar d1 d2 = chr (d1+16 + d2)
    hexDigit =
        (subtract (ord '0') . ord) <$> digit <|>
        ((+10) . subtract (ord 'a') . ord) <$> satisfy (`elem` "abcdef") <|>
        ((+10) . subtract (ord 'A') . ord) <$> satisfy (`elem` "ABCDEF")
    ordinary c = c /= '\\' && c /= '"'

solve1 :: [String] -> Int
solve1 lits = sum [length lit - length (unescape lit) | lit <- lits]

-- Part Two --

escape :: String -> String
escape s = '"' : concatMap escapeChar s ++ "\""
  where
    escapeChar c
      | isSpecial c = ['\\', c]
      | isAscii c = [c]
      | ord c < 16 = "\\x0" ++ showHex (ord c) ""
      | otherwise = "\\x" ++ showHex (ord c) ""
    isSpecial c = c == '\\' || c == '"'

solve2 :: [String] -> Int
solve2 lits = sum [length (escape lit) - length lit | lit <- lits]

main :: IO ()
main = do
    s <- readFile "input/08.txt"
    let ls = lines s
    print (solve1 ls)
    print (solve2 ls)
