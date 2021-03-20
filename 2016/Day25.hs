module Main where

{-
The code is equivalent to

    a = a + c*b;
    do {
        out(a%2);
        a = a/2;
    } while (a != 0);

So to yield an alternating string 0101..., a+c*b must be of the form
1010...10 (binary), i.e. (4^k-1)*2/3 for k > 0.
-}

-- extract first assignment of the register from the input
initValue :: String -> String -> Int
initValue r s =
    head [read v | ["cpy", v, dest] <- map words (lines s), dest == r]

solve1 :: Int -> Int -> Int
solve1 c b =
    head $ dropWhile (<= 0) [(4^k-1)*2 `div` 3 - c*b | k <- [(1::Int)..]]

main :: IO ()
main = do
    s <- readFile "input/25.txt"
    let c = initValue "c" s
    let b = initValue "b" s
    print (solve1 c b)
