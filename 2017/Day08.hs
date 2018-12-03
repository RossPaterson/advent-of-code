module Main where

import Parser
import Utilities
import Control.Applicative
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map

type Reg = String
data Op = Inc | Dec
    deriving Show
data Rel = Less | Greater | Equal | LessEq | GreaterEq | NotEqual
    deriving Show
data Action = Action Reg Op Int
    deriving Show
data Condition = Condition Reg Rel Int
    deriving Show
data Instruction = Instruction Action Condition
    deriving Show
type Input = [Instruction]

parse :: String -> Input
parse = map (runParser instruction) . lines
  where
    instruction = Instruction <$> action <* string " if " <*> condition
    action = Action <$> reg <* string " " <*> op <* string " " <*> int
    condition = Condition <$> reg <* string " " <*> rel <* string " " <*> int
    reg = some (satisfy isLower)
    op = Inc <$ string "inc" <|> Dec <$ string "dec"
    rel =
        Less <$ string "<" <|>
        Greater <$ string ">" <|>
        Equal <$ string "==" <|>
        LessEq <$ string "<=" <|>
        GreaterEq <$ string ">=" <|>
        NotEqual <$ string "!="

type State = Map String Int

initState :: State
initState = Map.empty

apply :: State -> Instruction -> State
apply s (Instruction act cond)
  | test s cond = update s act
  | otherwise = s

test :: State -> Condition -> Bool
test s (Condition r Less v) = value s r < v
test s (Condition r Greater v) = value s r > v
test s (Condition r Equal v) = value s r == v
test s (Condition r LessEq v) = value s r <= v
test s (Condition r GreaterEq v) = value s r >= v
test s (Condition r NotEqual v) = value s r /= v

value :: State -> Reg -> Int
value s r = Map.findWithDefault 0 r s

update :: State -> Action -> State
update s (Action r Inc v) = Map.insert r (value s r + v) s
update s (Action r Dec v) = Map.insert r (value s r - v) s

largest :: State -> Int
largest = maximum . map snd . Map.toList

solve1 :: Input -> Int
solve1 = largest . foldl apply initState

testInput :: String
testInput =
    "b inc 5 if a > 1\n\
    \a inc 1 if b < 5\n\
    \c dec -10 if a >= 1\n\
    \c inc -20 if c == 10\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 1)]

-- Part Two

solve2 :: Input -> Int
solve2 = maximum . map largest . dropWhile Map.null . scanl apply initState

tests2 :: [(String, Int)]
tests2 = [(testInput, 10)]

main :: IO ()
main = do
    s <- readFile "input08.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
