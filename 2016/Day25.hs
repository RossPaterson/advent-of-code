module Main where

{-
The code is equivalent to

    a = a + 7*362;
    do {
        out(a%2);
        a = a/2;
    } while (a != 0);

So to yield an alternating string 0101..., a+2534 must be of the form
1010...10 (binary), i.e. (4^k-1)*2/3 for k > 0.
-}

solve1 :: Int
solve1 = head $ dropWhile (<= 0) [(4^k-1)*2 `div` 3 - 2534 | k <- [(1::Int)..]]

main :: IO ()
main = print solve1
