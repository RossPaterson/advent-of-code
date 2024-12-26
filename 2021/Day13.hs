module Main where

import Utilities
import Geometry
import Parser
import Control.Applicative
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Input processing

data Fold = FoldX Int | FoldY Int
    deriving Show
type Input = ([Position], [Fold])

parse :: String -> Input
parse s = case paragraphs s of
    [sp, sf] ->
        (map (runParser pos) (lines sp), map (runParser fold_instr) (lines sf))
    _ -> error "bad input"
  where
    pos = Position <$> nat <* char ',' <*> nat
    fold_instr =
        FoldX <$ string "fold along x="  <*> nat <|>
        FoldY <$ string "fold along y="  <*> nat

-- Part One

-- fold values above the pivot down
foldInt :: Int -> Int -> Int
foldInt pivot x = pivot - abs (pivot - x)

foldPos :: Fold -> Position -> Position
foldPos (FoldX xf) (Position x y) = Position (foldInt xf x) y
foldPos (FoldY yf) (Position x y) = Position x (foldInt yf y)

foldPaper :: [Position] -> Fold -> [Position]
foldPaper ps f = map (foldPos f) ps

solve1 :: Input -> Int
solve1 (ps, f:_) = Set.size $ Set.fromList $ foldPaper ps f
solve1 (_, []) = error "no folds"

testInput :: String
testInput = "\
    \6,10\n\
    \0,14\n\
    \9,10\n\
    \0,3\n\
    \10,4\n\
    \4,11\n\
    \6,0\n\
    \6,12\n\
    \4,1\n\
    \0,13\n\
    \10,12\n\
    \3,4\n\
    \3,0\n\
    \8,4\n\
    \1,10\n\
    \2,14\n\
    \8,10\n\
    \9,0\n\
    \\n\
    \fold along y=7\n\
    \fold along x=5\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 17)]

-- Part Two

solve2 :: Input -> String
solve2 (ps, fs) =
    showGrid '.' (Map.fromList [(p, '#') | p <- foldl foldPaper ps fs])

tests2 :: [(String, String)]
tests2 = [(testInput, "\
    \#####\n\
    \#...#\n\
    \#...#\n\
    \#...#\n\
    \#####\n\
    \")]

main :: IO ()
main = do
    s <- readFile "input/13.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    putStr (solve2 input)
