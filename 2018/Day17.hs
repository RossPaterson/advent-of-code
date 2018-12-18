module Main where

import Parser
import Utilities
import Control.Applicative
import Data.Functor
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = Set Position

data Dir = Horiz | Vert
  deriving Show

parse :: String -> Input
parse = Set.fromList . concatMap (runParser vein) . lines
  where
    vein = mkVein <$> dir <* string "=" <*> nat <* string ", " <*
        dir <* string "=" <*> nat <* string ".." <*> nat
    dir = Horiz <$ char 'y' <|> Vert <$ char 'x'

type Position = (Int, Int)

mkVein :: Dir -> Int -> Int -> Int -> [Position]
mkVein Vert x y1 y2 = [(x, y) | y <- [y1..y2]]
mkVein Horiz y x1 x2 = [(x, y) | x <- [x1..x2]]

spring :: Position
spring = (500, 0)

-- Part One

solve1 :: Input -> Int
solve1 = fst . solve

solve :: Set Position -> (Int, Int)
solve clay = (ns+nr, ns)
  where
    ymin = minimum (map snd (Set.toList clay))
    Water standing running = maxGrowth spring clay
    ns = Set.size standing
    nr = Set.size (Set.filter inRange running)
    inRange (x, y) = y >= ymin

maxGrowth :: Position -> Input -> Water
maxGrowth start clay = convergeBy (same totalSize) (growth start clay)

growth :: Position -> Input -> [Water]
growth start clay = iterate (grow ymax clay) initWater
  where
    ymax = maximum (map snd (Set.toList clay))
    initWater = Water Set.empty (Set.singleton start)

-- positions with standing or running water
data Water = Water (Set Position) (Set Position)
  deriving (Show, Eq)

totalSize :: Water -> Int
totalSize (Water standing running) = Set.size standing + Set.size running

showState :: Position -> Set Position -> Water -> String
showState start clay (Water standing running) =
    unlines [[showPos (x, y) | x <- [xmin-1..xmax+1]] | y <- [0..ymax]]
  where
    xmin = minimum (map fst (Set.toList clay))
    xmax = maximum (map fst (Set.toList clay))
    ymin = minimum (map snd (Set.toList clay))
    ymax = maximum (map snd (Set.toList clay))
    showPos p
      | p == start = '+'
      | Set.member p clay = '#'
      | Set.member p standing = '~'
      | Set.member p running = '|'
      | otherwise = '.'

-- a horizontal layer of water, at given y-coordinate
data Layer = Layer Int End End
-- the end of a layer: open if the position below isn't blocked,
-- otherwise closed if the next position is blocked
data End = End EndType Int
  deriving Show
data EndType = Open | Closed
  deriving Show

grow :: Int -> Set Position -> Water -> Water
grow ymax clay (Water standing running) =
    foldl addLayer (Water standing running') layers
  where
    blocked = Set.union clay standing
    running' =
        (Set.filter inRange (below running) `Set.difference` blocked)
        `Set.union` running
    layers = map (mkLayer blocked) (Set.toList running')
    inRange (x, y) = y <= ymax

-- the positions immediately below those in s
below :: Set Position -> Set Position
below s = Set.fromList [(x, y+1) | (x, y) <- Set.toList s]

-- extend (x,y) into a horizontal layer of water
mkLayer :: Set Position -> Position -> Layer
mkLayer blocked (x, y) =
    Layer y (mkEnd (iterate (subtract 1) x)) (mkEnd (iterate (+1) x))
  where
    mkEnd (x1:x2:xs)
      | not (Set.member (x1, y+1) blocked) = End Open x1
      | Set.member (x2, y) blocked = End Closed x1
      | otherwise = mkEnd (x2:xs)

-- the positions of a layer
layerPositions :: Layer -> Set Position
layerPositions (Layer y (End _ x1) (End _ x2)) =
    Set.fromList [(x, y) | x <- [x1..x2]]

-- add a layer of standing or running water
addLayer :: Water -> Layer -> Water
addLayer (Water standing running) l
  | closed l = Water (Set.union standing ps) (Set.difference running ps)
  | otherwise = Water standing (Set.union running ps)
  where
    ps = layerPositions l

-- a layer will hold standing water if both ends are closed
closed :: Layer -> Bool
closed (Layer _ (End Closed _) (End Closed _)) = True
closed _ = False

tests1 :: [(String, Int)]
tests1 = [(testInput, 57)]

testInput :: String
testInput = "\
\x=495, y=2..7\n\
\y=7, x=495..501\n\
\x=501, y=3..7\n\
\x=498, y=2..4\n\
\x=506, y=1..2\n\
\x=498, y=10..13\n\
\x=504, y=10..13\n\
\y=13, x=498..504\n"

-- Part Two

solve2 :: Input -> Int
solve2 = snd . solve

tests2 :: [(String, Int)]
tests2 = [(testInput, 29)]

main :: IO ()
main = do
    s <- readFile "input17.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve input)
