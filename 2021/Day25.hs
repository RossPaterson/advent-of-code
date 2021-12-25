module Main where

import Utilities
import Geometry
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

data State = State {
    south :: Set Position,
    east :: Set Position,
    width :: Int,
    height :: Int }
    deriving Show

type Input = State

parse :: String -> Input
parse s = State {
    south = Set.fromList [p | (p, c) <- pcs, c == 'v'],
    east = Set.fromList [p | (p, c) <- pcs, c == '>'],
    width = maximum [x | (Position x _, _) <- pcs] + 1,
    height = maximum [y | (Position _ y, _) <- pcs] + 1 }
  where
    pcs = readGrid s

-- Part One

moveEast :: State -> Position -> Position
moveEast s (Position x y) = Position ((x+1) `mod` width s) y

moveSouth :: State -> Position -> Position
moveSouth s (Position x y) = Position x ((y+1) `mod` height s)

allMoves :: State -> (State -> Position -> Position) -> Set Position ->
    Maybe (Set Position)
allMoves s move ps
  | null moves = Nothing
  | otherwise = Just (Set.union new (Set.difference ps old))
  where
    moves = [(p, p') |
        p <- Set.elems ps,
        let p' = move s p,
        not (Set.member p' (east s)),
        not (Set.member p' (south s))]
    old = Set.fromList (map fst moves)
    new = Set.fromList (map snd moves)

moveHerdEast :: State -> Maybe State
moveHerdEast s =
    fmap (\ ps -> s { east = ps }) (allMoves s moveEast (east s))

moveHerdSouth :: State -> Maybe State
moveHerdSouth s =
    fmap (\ ps -> s { south = ps }) (allMoves s moveSouth (south s))

step :: State -> Maybe State
step s = case moveHerdEast s of
    Nothing -> moveHerdSouth s
    Just s' -> case moveHerdSouth s' of
        Nothing -> Just s'
        Just s'' -> Just s''

solve1 :: Input -> Int
solve1 = length . iterateWhileJust step

testInput :: String
testInput = "\
    \v...>>.vv>\n\
    \.vv>>.vv..\n\
    \>>.>v>...v\n\
    \>>v>>.>.v.\n\
    \v>v.vv.v..\n\
    \>.>>..v...\n\
    \.vv..>.>v.\n\
    \v.v..>>v.v\n\
    \....v..v.>\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 58)]

-- there is no Part Two on Day 25

main :: IO ()
main = do
    s <- readFile "input/25.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
