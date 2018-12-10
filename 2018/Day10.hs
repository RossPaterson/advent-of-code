module Main where

import Parser
import Utilities
import Control.Applicative
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = State

-- simple particle system
type State = [Particle]
data Particle = Particle { pos :: Vector, vel :: Vector }
  deriving Show
type Vector = (Int, Int)

parse :: String -> Input
parse = map (runParser particle) . lines
  where
    particle = Particle <$ string "position=" <*> point <*
        string " velocity=" <*> point
    point = (,) <$ char '<' <*> coord <* char ',' <*> coord <* char '>'
    coord = many space *> int

-- Both parts

-- String displaying all the candidate states and when they occur.
-- There might be more than one, and some may even be more compact than
-- the one we can read -- we'll need to visually inspect the result.
solve :: Input -> String
solve = concat . map (uncurry display) . candidates

-- States that might be compact enough to contain a message.
-- We expect the points to converge into a compact group and then expand
-- again as each of them keeps moving linearly.
candidates :: State -> [(Int, State)]
candidates s =
    takeWhile grouped $
    dropWhile (not . grouped) $
    zip [0..] $ iterate (map move) s
  where
    grouped = compact . map pos . snd

-- move a particle for one tick
move :: Particle -> Particle
move (Particle (px, py) (vx, vy)) = Particle (px+vx, py+vy) (vx, vy)

-- is the bounding rectangle small enough to be a message?
compact :: [Vector] -> Bool
compact ps = width rect < 80 && height rect <= 12
  where
    rect = boundingRect ps

-- show a numbered state
display :: Int -> State -> String
display n s = show n ++ ":\n" ++ displayPoints (map pos s) ++ "\n"

-- display the filled part of a state as a bitmap
displayPoints :: [Vector] -> String
displayPoints ps =
    unlines [[if Set.member (x, y) pset then '#' else '.' |
        x <- [xmin..xmax]] | y <- [ymin..ymax]]
  where
    Rect (xmin, ymin) (xmax, ymax) = boundingRect ps
    pset = Set.fromList ps

-- rectangles, described by a bottom left and top right corner
data Rect = Rect Vector Vector
  deriving Show

width :: Rect -> Int
width (Rect (xmin, ymin) (xmax, ymax)) = xmax - xmin + 1

height :: Rect -> Int
height (Rect (xmin, ymin) (xmax, ymax)) = ymax - ymin + 1

-- smallest rectangle containing all the points
boundingRect :: [Vector] -> Rect
boundingRect ps = Rect (xmin, ymin) (xmax, ymax)
  where
    xmin = minimum (map fst ps)
    xmax = maximum (map fst ps)
    ymin = minimum (map snd ps)
    ymax = maximum (map snd ps)

testInput :: String
testInput = "\
\position=< 9,  1> velocity=< 0,  2>\n\
\position=< 7,  0> velocity=<-1,  0>\n\
\position=< 3, -2> velocity=<-1,  1>\n\
\position=< 6, 10> velocity=<-2, -1>\n\
\position=< 2, -4> velocity=< 2,  2>\n\
\position=<-6, 10> velocity=< 2, -2>\n\
\position=< 1,  8> velocity=< 1, -1>\n\
\position=< 1,  7> velocity=< 1,  0>\n\
\position=<-3, 11> velocity=< 1, -2>\n\
\position=< 7,  6> velocity=<-1, -1>\n\
\position=<-2,  3> velocity=< 1,  0>\n\
\position=<-4,  3> velocity=< 2,  0>\n\
\position=<10, -3> velocity=<-1,  1>\n\
\position=< 5, 11> velocity=< 1, -2>\n\
\position=< 4,  7> velocity=< 0, -1>\n\
\position=< 8, -2> velocity=< 0,  1>\n\
\position=<15,  0> velocity=<-2,  0>\n\
\position=< 1,  6> velocity=< 1,  0>\n\
\position=< 8,  9> velocity=< 0, -1>\n\
\position=< 3,  3> velocity=<-1,  1>\n\
\position=< 0,  5> velocity=< 0, -1>\n\
\position=<-2,  2> velocity=< 2,  0>\n\
\position=< 5, -2> velocity=< 1,  2>\n\
\position=< 1,  4> velocity=< 2,  1>\n\
\position=<-2,  7> velocity=< 2, -2>\n\
\position=< 3,  6> velocity=<-1, -1>\n\
\position=< 5,  0> velocity=< 1,  0>\n\
\position=<-6,  0> velocity=< 2,  0>\n\
\position=< 5,  9> velocity=< 1, -2>\n\
\position=<14,  7> velocity=<-2,  0>\n\
\position=<-3,  6> velocity=< 2, -1>\n"

main :: IO ()
main = do
    s <- readFile "input10.txt"
    let input = parse s
    putStr $ solve input
