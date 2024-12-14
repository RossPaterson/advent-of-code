module Main where

import Geometry
import Graph
import Parser
import Utilities
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = [Robot]

data Robot = Robot { pos :: Position, vel :: Position }
    deriving Show

parse :: String -> Input
parse = map (runParser robot) . lines
  where
    robot = Robot <$ string "p=" <*> position <* string " v=" <*> position
    position = Position <$> int <* char ',' <*> int

-- Part One

solve1 :: Input -> Int
solve1 robots = safety box $ map pos $ times 100 (map (move box)) robots
  where
    box = room robots

-- the room is determined by the initial position of the robots
room :: [Robot] -> AABox Position
room = boundingBox . map pos

move :: AABox Position -> Robot -> Robot
move box (Robot p v) = Robot (wrap box (p .+. v)) v

wrap :: AABox Position -> Position -> Position
wrap box (Position x y) =
    Position ((x - min_x) `mod` len_x + min_x)
        ((y - min_y) `mod` len_y + min_y)
  where
    len_x = max_x - min_x + 1
    len_y = max_y - min_y + 1
    Position min_x min_y = minCorner box
    Position max_x max_y = maxCorner box

-- product of the number of positions in each quadrant
safety :: AABox Position -> [Position] -> Int
safety box ps =
    product [n |
        (_, n) <- frequency [(comp_x, comp_y) |
            Position x y <- ps,
            let comp_x = compare x mid_x, comp_x /= EQ,
            let comp_y = compare y mid_y, comp_y /= EQ]]
  where
    mid_x = (min_x + max_x) `div` 2
    mid_y = (min_y + max_y) `div` 2
    Position min_x min_y = minCorner box
    Position max_x max_y = maxCorner box

testInput :: String
testInput = "\
    \p=0,4 v=3,-3\n\
    \p=6,3 v=-1,-3\n\
    \p=10,3 v=-1,2\n\
    \p=2,0 v=2,-1\n\
    \p=0,0 v=1,3\n\
    \p=3,0 v=-2,-2\n\
    \p=7,6 v=-1,-3\n\
    \p=3,0 v=-1,-2\n\
    \p=9,3 v=2,3\n\
    \p=7,3 v=-1,2\n\
    \p=2,4 v=2,-3\n\
    \p=9,5 v=-3,-3\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 12)]

-- Part Two

-- Size of region to look for.
-- Anything between 16 and 229 would work on my input.
large :: Int
large = 200

-- How many steps until the image contains a large blob?
solve2 :: Input -> Int
solve2 robots =
    length $
    takeWhile (\ rs -> largestRegionSize (map pos rs) < large) $
    iterate (map (move box)) robots
  where
    box = room robots

largestRegionSize :: [Position] -> Int
largestRegionSize = maximum . map Set.size . regions . Set.fromList

-- all the contiguous regions of the set
regions :: Set Position -> [Set Position]
regions = unfoldr getRegion

getRegion :: Set Position -> Maybe (Set Position, Set Position)
getRegion ps = do
    p <- Set.lookupMin ps
    let r = Set.fromList (concat (bfs next [p]))
    return (r, Set.difference ps r)
  where
    next p = filter (flip Set.member ps) [p .+. d | d <- unitVectors]

-- visualization
showRobots :: AABox Position -> [Robot] -> String
showRobots box robots = showBox box showPos
  where
    showPos p
      | Set.member p locs = '#'
      | otherwise = '.'
    locs = Set.fromList (map pos robots)

main :: IO ()
main = do
    s <- readFile "input/14.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    print (solve2 input)
