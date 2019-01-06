module Main where

import Parser
import Utilities
import Control.Applicative
import Data.Functor
import Data.Semigroup
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

-- a vein of clay
mkVein :: Dir -> Int -> Int -> Int -> [Position]
mkVein Vert x y1 y2 = [(x, y) | y <- [y1..y2]]
mkVein Horiz y x1 x2 = [(x, y) | x <- [x1..x2]]

-- position of the spring
spring :: Position
spring = (500, 0)

-- Part One

solve1 :: Input -> Int
solve1 = fst . solve

solve :: Set Position -> (Int, Int)
solve clay = (ns+nr, ns)
  where
    ymin = minimum (map snd (Set.toList clay))
    w = maxFlow spring clay
    ns = Set.size (standing w)
    nr = Set.size (Set.filter inRange (running w))
    inRange (x, y) = y >= ymin

-- all the positions reached by water when it reaches a steady state
maxFlow :: Position -> Input -> Water
maxFlow start clay = incremental (flow ymax clay) initWater
  where
    ymax = maximum (map snd (Set.toList clay))
    initWater = runningWater (Set.singleton start)

-- incrementally compute the fixed point of a function f satisfying:
--     f mempty = x0
--     f x = x <> dx, and f (x <> dx) = x <> dx <> g x dx
--     g x mempty = mempty
incremental :: (Monoid a, Eq a) => (a -> a -> a) -> a -> a
incremental g = loop mempty
  where
    loop x dx
      | dx == mempty = x
      | otherwise = loop (x <> dx) (g x dx)

-- all the steps leading to an incrementally computed fixed point
incrementalSteps :: (Monoid a, Eq a) => (a -> a -> a) -> a -> [a]
incrementalSteps g = loop mempty
  where
    loop x dx
      | dx == mempty = [x]
      | otherwise = x : loop (x <> dx) (g x dx)

-- positions with all water and standing water
data Water = Water { water :: Set Position, standing :: Set Position }
  deriving (Show, Eq)

instance Semigroup Water where
    Water w1 s1 <> Water w2 s2 = Water (Set.union w1 w2) (Set.union s1 s2)

instance Monoid Water where
    mempty = Water Set.empty Set.empty

-- the water that is not standing
running :: Water -> Set Position
running w = Set.difference (water w) (standing w)

-- a set consisting of running water
runningWater :: Set Position -> Water
runningWater ps = Water ps Set.empty

-- a set consisting of standing water
standingWater :: Set Position -> Water
standingWater ps = Water ps ps

-- display the current state, per the puzzle description
showState :: Position -> Set Position -> Water -> String
showState start clay w =
    unlines [[showPos (x, y) | x <- [xmin-1..xmax+1]] | y <- [0..ymax]]
  where
    xmin = minimum (map fst positions)
    xmax = maximum (map fst positions)
    ymin = minimum (map snd positions)
    ymax = maximum (map snd positions)
    positions = spring:Set.toList clay
    showPos p
      | p == start = '+'
      | Set.member p clay = '#'
      | Set.member p (standing w) = '~'
      | Set.member p (water w) = '|'
      | otherwise = '.'

-- expand water by one step, given the previous state and previous expansion
flow :: Int -> Set Position -> Water -> Water -> Water
flow ymax clay w dw =
    runningWater newrunning <>
    mconcat [layerWater (mkLayer blocked p) | p <- Set.toList spreading]
  where
    inRange (x, y) = y <= ymax
    -- water runs into empty space below previously new running water
    newrunning =
        Set.filter inRange (Set.map below (running dw))
            `Set.difference` water w
            `Set.difference` water dw
            `Set.difference` clay
    w' = w <> dw
    -- barrier to running water
    blocked = clay `Set.union` standing w'
    -- running water that will spread out horizontally
    spreading =
        -- new running water above clay or standing water
        Set.map above (Set.map below newrunning `Set.intersection` blocked)
        `Set.union`
        -- running water above new standing water
        (running w' `Set.intersection` Set.map above (standing dw))

-- the position immediately below
below :: Position -> Position
below (x, y) = (x, y+1)

-- the position immediately above
above :: Position -> Position
above (x, y) = (x, y-1)

-- a horizontal layer of water, at given y-coordinate
data Layer = Layer Int End End
-- the end of a layer at an x-coordinate: open if the position below
-- isn't blocked, otherwise closed if the next position is blocked
data End = End EndType Int
  deriving Show
data EndType = Open | Closed
  deriving Show

-- extend (x,y) into a horizontal layer of water
mkLayer :: Set Position -> Position -> Layer
mkLayer blocked (x, y) =
    Layer y (mkEnd (iterate (subtract 1) x)) (mkEnd (iterate (+1) x))
  where
    mkEnd (x1:x2:xs)
      | not (Set.member (x1, y+1) blocked) = End Open x1
      | Set.member (x2, y) blocked = End Closed x1
      | otherwise = mkEnd (x2:xs)

-- a layer of standing or running water
layerWater :: Layer -> Water
layerWater l
  | closed l = standingWater ps
  | otherwise = runningWater ps
  where
    ps = layerPositions l

-- a layer will hold standing water if both ends are closed
closed :: Layer -> Bool
closed (Layer _ (End Closed _) (End Closed _)) = True
closed _ = False

-- the positions of a layer
layerPositions :: Layer -> Set Position
layerPositions (Layer y (End _ x1) (End _ x2)) =
    Set.fromList [(x, y) | x <- [x1..x2]]

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
    s <- readFile "input/17.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve input)
