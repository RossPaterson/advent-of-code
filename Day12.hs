module Main where

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
        Array <$ char '[' <*> (value `sepBy1` char ',') <* char ']' <|>
        Object <$ char '{' <*> (field `sepBy1` char ',') <* char '}' <|>
        Number <$> int <|>
        String <$> literal
    field = (,) <$> literal <* char ':' <*> value
    literal = char '"' *> many (satisfy (/= '"')) <* char '"'

total :: Value -> Int
total (Array vs) = sum (map total vs)
total (Object fs) = sum (map (total . snd) fs)
total (Number n) = n
total (String _) = 0

solve1 :: Input -> Int
solve1 = total

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

main :: IO ()
main = do
    s <- readFile "input12.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
