module Main where

type Grid a = [[a]]
type Input = Grid Bool

parse :: String -> Input
parse = map (map (== '#')) . lines

showGrid :: Grid Bool -> String
showGrid = unlines . map (map light)
  where
    light True = '#'
    light False = '.'

numLights :: Grid Bool -> Int
numLights = length . filter id . concat

-- Conway's Game of Life
rule :: Bool -> Int -> Bool
rule True n = n == 2 || n == 3
rule False n = n == 3

step :: Grid Bool -> Grid Bool
step bss = map (map newValue) neighbourhoods
  where
    newValue :: Triple (Triple Bool) -> Bool
    newValue ((a,b,c), (d,e,f), (g,h,i)) =
        rule e (length (filter id [a, b, c, d, f, g, h, i]))

    neighbourhoods :: Grid (Triple (Triple Bool))
    neighbourhoods = threesWith zip3 (map threes padded)

    padded = pad blank (map (pad False) bss)
    pad x xs = x : (xs ++ [x])
    blank = replicate (width+2) False
    width = length (head bss)

type Triple a = (a,a,a)

threesWith :: (a -> a -> a -> b) -> [a] -> [b]
threesWith f xs = zipWith3 f xs (tail xs) (tail (tail xs))

threes :: [a] -> [Triple a]
threes = threesWith (,,)

history :: Grid Bool -> [Grid Bool]
history = iterate step

solve1 :: Input -> Int
solve1 = numLights . (!!100) . history

test :: String
test =
    ".#.#.#\n\
    \...##.\n\
    \#....#\n\
    \..#...\n\
    \#.#..#\n\
    \####..\n"

-- Part Two --

stuckLights :: Grid Bool -> Grid Bool
stuckLights bss =
    endsOn (head bss) : (init (tail bss) ++ [endsOn (last bss)])
  where
    endsOn bs = True : (init (tail bs) ++ [True])

history2 :: Grid Bool -> [Grid Bool]
history2 = iterate (stuckLights . step) . stuckLights

solve2 :: Input -> Int
solve2 = numLights . (!!100) . history2

main :: IO ()
main = do
    s <- readFile "input18.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
