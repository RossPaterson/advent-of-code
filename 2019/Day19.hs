module Main where

import Intcode

-- Input processing

type Input = Memory

parse :: String -> Input
parse = readMemory

-- Part One

-- test whether (x, y) is within the beam implemented by mem
inBeam :: Memory -> Int -> Int -> Bool
inBeam mem = test
  where
    program = streamFunction mem -- cached for sharing
    test x y = fromValue $ head $ program [toValue x, toValue y]

-- show a rectangular slice of the beam
showBeam :: Memory -> Int -> Int -> String
showBeam mem w h =
    unlines [[showPoint (test x y) | x <- [0..w-1]] | y <- [0..h-1]]
  where
    test = inBeam mem
    showPoint True = '#'
    showPoint False = '.'

solve1 :: Input -> Int
solve1 mem =
    length $ filter id $ [inBeam mem x y | x <- [0..49], y <- [0..49]]

-- Part Two

-- for each horizontal slice of the beam, the x and y coordinates of
-- the first point and the width of the slice
slices :: Memory -> [(Int, Int, Int)]
slices mem = iterate next (3, 3, 0)
  where
    next (x, y, w) = (x', y', w')
      where
         y' = y+1
         -- the slice cannot start earlier than the last one
         x' = head $ filter (flip test y') [x..]
         -- not necessarily as wide as the previous slice, but
         -- the slice cannot end earlier than the last one
         w' = length (takeWhile (flip test y') [x+w..]) + w - x' + x
    test = inBeam mem

-- top left corner of the first wxh rectangle that is within the beam
corner :: Memory -> Int -> Int -> (Int, Int)
corner mem w h = head [(x2, y1) |
    ((x1, y1, w1), (x2, _y2, _w2)) <- zip rs (drop (h-1) rs),
    x2 - x1 + w <= w1]
  where
    rs = slices mem

solve2 :: Input -> Int
solve2 mem = x*10000 + y
  where
    (x, y) = corner mem 100 100

main :: IO ()
main = do
    s <- readFile "input/19.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
