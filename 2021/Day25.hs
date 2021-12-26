module Main where

import Utilities
import Geometry
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

data State = State {
    east :: Set Position,
    south :: Set Position,
    width :: Int,
    height :: Int }
    deriving Show

type Input = State

parse :: String -> Input
parse s = State {
    east = Set.fromList [p | (p, c) <- pcs, c == '>'],
    south = Set.fromList [p | (p, c) <- pcs, c == 'v'],
    width = maximum [x | (Position x _, _) <- pcs] + 1,
    height = maximum [y | (Position _ y, _) <- pcs] + 1 }
  where
    pcs = readGrid s

-- Part One

setEast :: State -> Set Position -> State
setEast s ps = s { east = ps }

setSouth :: State -> Set Position -> State
setSouth s ps = s { south = ps }

moveEast :: State -> Position -> Position
moveEast s (Position x y) = Position ((x+1) `mod` width s) y

moveSouth :: State -> Position -> Position
moveSouth s (Position x y) = Position x ((y+1) `mod` height s)

moveSet :: State -> (State -> Position -> Position) ->
    Set Position -> Maybe (Set Position)
moveSet s movePos ps
  | null moves = Nothing
  | otherwise = Just (Set.union new (Set.difference ps old))
  where
    moves = [(p, p') |
        p <- Set.elems ps,
        let p' = movePos s p,
        not (Set.member p' (east s)),
        not (Set.member p' (south s))]
    old = Set.fromList (map fst moves)
    new = Set.fromList (map snd moves)

moveHerdEast :: State -> Maybe State
moveHerdEast s = fmap (setEast s) (moveSet s moveEast (east s))

moveHerdSouth :: State -> Maybe State
moveHerdSouth s = fmap (setSouth s) (moveSet s moveSouth (south s))

-- apply two possible changes in sequence
andThen :: (a -> Maybe a) -> (a -> Maybe a) -> a -> Maybe a
andThen f g x = case f x of
    Nothing -> g x
    Just y -> case g y of
        Nothing -> Just y
        Just z -> Just z

step :: State -> Maybe State
step = moveHerdEast `andThen` moveHerdSouth

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
