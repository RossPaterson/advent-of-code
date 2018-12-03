module Main where

import Parser
import Control.Applicative

data Box = Box Int Int Int
type Input = [Box]

parse :: String -> Input
parse = map (runParser box) . lines
  where
    box = Box <$> int <* char 'x' <*> int <* char 'x' <*> int

wrapping :: Box -> Int
wrapping (Box l w h) = 2*l*w + 2*w*h + 2*h*l + minimum [l*w, w*h, h*l]

solve1 :: Input -> Int
solve1 = sum . map wrapping

test = "2x3x4\n1x1x10\n"

-- Part Two --

ribbon :: Box -> Int
ribbon (Box l w h) = l*w*h + 2*minimum [l+w, w+h, h+l]

solve2 :: Input -> Int
solve2 = sum . map ribbon

main :: IO ()
main = do
    s <- readFile "input02.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
