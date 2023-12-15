module Main where

import Parser
import Utilities
import Control.Applicative
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map

-- Input processing (different for the two parts)

type Input1 = [String]

parse1 :: String -> Input1
parse1 s = lines [if c == ',' then '\n' else c | c <- s]

type Instruction = (Label, Operation)
data Operation = Remove | Assign Int
    deriving (Show)
type Label = String

type Input2 = [Instruction]

parse2 :: String -> Input2
parse2 = map (runParser instruction) . parse1
  where
    instruction = (,) <$> label <*> operation
    operation = Assign <$ char '=' <*> nat <|> Remove <$ char '-'
    label = some letter

-- Part One

hash :: String -> Int
hash = foldl hashChar 0

hashChar :: Int -> Char -> Int
hashChar n c = (n + ord c)*17 `mod` 256

solve1 :: Input1 -> Int
solve1 = sum . map hash

testInput :: String
testInput = "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 1320)]

-- Part Two

type Lens = (Label, Int)
type BoxArray = Map Int [Lens]

-- array of empty boxes
initBoxArray :: BoxArray
initBoxArray = Map.fromList [(k, []) | k <- [0..255]]

-- each step applies a label and operation to one of the boxes
step :: BoxArray -> Instruction -> BoxArray
step boxes (label, op) =
    Map.adjust (applyOperation label op) (hash label) boxes

applyOperation :: Label -> Operation -> [Lens] -> [Lens]
applyOperation label Remove ls = filter (not . hasLabel label) ls
applyOperation label (Assign n) ls
  | any (hasLabel label) ls =
    [(l, if l == label then n else old_n) | (l, old_n) <- ls]
  | otherwise = ls ++ [(label, n)]

hasLabel :: Label -> Lens -> Bool
hasLabel label (l, _) = l == label

focussingPower :: BoxArray -> Map Label Int
focussingPower boxes =
    Map.fromListWith (*) [(l, (box_no+1) * lens_no * n) |
        (box_no, ls) <- Map.assocs boxes,
        (lens_no, (l, n)) <- zip [1..] ls]

solve2 :: Input2 -> Int
solve2 = sum . Map.elems . focussingPower . foldl step initBoxArray

tests2 :: [(String, Int)]
tests2 = [(testInput, 145)]

main :: IO ()
main = do
    s <- readFile "input/15.txt"
    let input1 = parse1 s
    putStr (unlines (failures "solve1" (solve1 . parse1) tests1))
    print (solve1 input1)
    let input2 = parse2 s
    putStr (unlines (failures "solve2" (solve2 . parse2) tests2))
    print (solve2 input2)
