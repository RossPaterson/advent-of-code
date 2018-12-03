module Main where

import Utilities
import Parser
import Control.Applicative
import Data.Char
import Data.List
import Data.Maybe

type Element = String
type Molecule = [Element]
data Rule = Element :=> Molecule
  deriving Show
data Input = Input [Rule] Molecule
  deriving Show

parse :: String -> Input
parse s = Input
    (map (runParser rule) (takeWhile (not . null) ls))
    (runParser molecule (last ls))
  where
    rule = (:=>) <$> lhs <* string " => " <*> molecule
    molecule = many element
    lhs = (:[]) <$> satisfy isLower <|> element
    element =
        (:) <$> satisfy isUpper <*> (maybeToList <$> optional (satisfy isLower))
    ls = lines s

replacements :: Input -> [Molecule]
replacements (Input rules es) = fast_nub $
    [front ++ repl ++ back |
        (front, e:back) <- zip (inits es) (tails es),
        (r :=> repl) <- rules, r == e]

solve1 :: Input -> Int
solve1 = length . replacements

test = "H => HO\n\
    \H => OH\n\
    \O => HH\n\
    \\n\
    \HOHOHO\n"

-- Part Two --

{-
All rules in the given input are of one of the forms

E => E E
E => E Rn E Ar
E => E Rn E Y E Ar
E => E Rn E Y E Y E Ar
-}

solve2 :: Input -> Int
solve2 (Input _ es) =
    length es - 1 - number "Rn" es - number "Ar" es - 2*number "Y" es

number :: Eq a => a -> [a] -> Int
number x = length . filter (==x)

main :: IO ()
main = do
    s <- readFile "input19.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
