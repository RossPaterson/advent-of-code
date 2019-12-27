module Main where

import Utilities
import Parser
import Control.Applicative

-- JSON values
data Value
    = Array [Value]
    | Object [(String, Value)]
    | Number Int
    | String String
  deriving (Show, Eq, Ord)
type Input = Value

-- lacks:
--   spaces between tokens
--   true, false, null
--   floating point numbers
--   backslash escapes in string literals
parse :: String -> Input
parse = runParser (value <* many space)
  where
    value =
        Array <$ char '[' <*> list value <* char ']' <|>
        Object <$ char '{' <*> list field <* char '}' <|>
        Number <$> int <|>
        String <$> literal
    field = (,) <$> literal <* char ':' <*> value
    literal = char '"' *> many (satisfy (/= '"')) <* char '"'
    list p = p `sepBy1` char ',' <|> pure []

total :: Value -> Int
total (Array vs) = sum (map total vs)
total (Object fs) = sum (map (total . snd) fs)
total (Number n) = n
total (String _) = 0

solve1 :: Input -> Int
solve1 = total

tests1 :: [(String, Int)]
tests1 = [
    ("[1,2,3]", 6),
    ("{\"a\":2,\"b\":4}", 6),
    ("[[[3]]]", 3),
    ("{\"a\":{\"b\":4},\"c\":-1}", 3),
    ("{\"a\":[-1,1]}", 0),
    ("[-1,{\"a\":1}]", 0),
    ("[]", 0),
    ("{}", 0)]

-- Part Two --

-- trim objects with property values "red"
prune :: Value -> Value
prune (Array vs) = Array (map prune vs)
prune (Object fs)
  | any ((== String "red") . snd) fs = Object []
  | otherwise = Object [(s, prune v) | (s, v) <- fs]
prune v = v

solve2 :: Input -> Int
solve2 = total . prune

tests2 :: [(String, Int)]
tests2 = [
    ("[1,2,3]", 6),
    ("[1,{\"c\":\"red\",\"b\":2},3]", 4),
    ("{\"d\":\"red\",\"e\":[1,2,3,4],\"f\":5}", 0),
    ("[1,\"red\",5]", 6)]

main :: IO ()
main = do
    s <- readFile "input/12.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
