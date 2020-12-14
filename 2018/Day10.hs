module Main where

import Geometry
import Parser
import Control.Applicative
import qualified Data.Map as Map

-- Input processing

type Input = System

-- simple particle system
type System = [Particle]
data Particle = Particle { pos :: Position, vel :: Position }
  deriving Show

parse :: String -> Input
parse = map (runParser particle) . lines
  where
    particle = Particle <$ string "position=" <*> point <*
        string " velocity=" <*> point
    point = Position <$ char '<' <*> coord <* char ',' <*> coord <* char '>'
    coord = many space *> int

-- Parts One and Two

-- String displaying all the candidate states and when they occur.
-- There might be more than one, and some may even be more compact than
-- the one we can read -- we'll need to visually inspect the result.
solve :: Input -> String
solve = concat . map (uncurry display) . candidates

-- States that might be compact enough to contain a message.
-- We expect the points to converge into a compact group and then expand
-- again as each of them keeps moving linearly.
candidates :: System -> [(Int, [Position])]
candidates s =
    takeWhile grouped $
    dropWhile (not . grouped)
    [(n, map (positionAt n) s) | n <- [0..]]
  where
    grouped = compact . snd

-- position of particle at time t
positionAt :: Int -> Particle -> Position
positionAt t (Particle (Position px py) (Position vx vy)) =
    Position (px+vx*t) (py+vy*t)

-- is the bounding rectangle small enough to be a message?
compact :: [Position] -> Bool
compact ps = width rect < 80 && height rect <= 12
  where
    rect = boundingRect ps

-- show a numbered state
display :: Int -> [Position] -> String
display n ps = displayPoints ps ++ show n ++ "\n"

-- display the filled part of a state as a bitmap
displayPoints :: [Position] -> String
displayPoints ps = showGrid '.' $ Map.fromList [(p, '#') | p <- ps]

-- rectangles, described by a bottom left and top right corner
data Rect = Rect Position Position
  deriving Show

width :: Rect -> Int
width (Rect (Position xmin _ymin) (Position xmax _ymax)) = xmax - xmin + 1

height :: Rect -> Int
height (Rect (Position _xmin ymin) (Position _xmax ymax)) = ymax - ymin + 1

-- smallest rectangle containing all the points
boundingRect :: [Position] -> Rect
boundingRect ps = Rect (Position xmin ymin) (Position xmax ymax)
  where
    xmin = minimum [x | Position x _ <- ps]
    xmax = maximum [x | Position x _ <- ps]
    ymin = minimum [y | Position _ y <- ps]
    ymax = maximum [y | Position _ y <- ps]

testInput :: String
testInput =
    "position=< 9,  1> velocity=< 0,  2>\n\
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
    s <- readFile "input/10.txt"
    let input = parse s
    putStr $ solve input
