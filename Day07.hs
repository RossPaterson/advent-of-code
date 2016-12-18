module Day07 where

import Parser
import Control.Applicative
import Data.List
import Data.Maybe

data Address = Address String [(String, String)]
  deriving Show
type Input = [Address]

parse :: String -> Input
parse = map getAddress . lines

getAddress :: String -> Address
getAddress = runParser $ Address <$> word <*> many ((,) <$> hyperword <*> word)
  where
    hyperword = char '[' *> word <* char ']'
    word = many (satisfy (\ c -> c /= '[' && c /= ']'))

supernet :: Address -> [String]
supernet (Address w hws) = w : map snd hws

hypernet :: Address -> [String]
hypernet (Address w hws) = map fst hws

supportsTLS :: Address -> Bool
supportsTLS addr =
    any hasABBA (supernet addr) && not (any hasABBA (hypernet addr))

hasABBA :: String -> Bool
hasABBA = any isABBA . tails

isABBA :: String -> Bool
isABBA (c1:c2:c3:c4:_) = c1 == c4 && c2 == c3 && c1 /= c2
isABBA _ = False

test = ["abba[mnop]qrst", "abcd[bddb]xyyx", "aaaa[qwer]tyui", "ioxxoj[asdfgh]zxcvbn"]

solve1 :: Input -> Int
solve1 = length . filter supportsTLS

-- Part Two --

supportsSSL :: Address -> Bool
supportsSSL addr = not $ null [bab |
    bab <- concatMap getBABs (supernet addr),
    length [s | h <- hypernet addr, s <- tails h, bab `isPrefixOf` s] == 1]

getBABs :: String -> [String]
getBABs = mapMaybe getBAB . tails

getBAB :: String -> Maybe String
getBAB (c1:c2:c3:_) | c1 == c3 && c1 /= c2 = Just [c2, c1, c2]
getBAB _ = Nothing

solve2 :: Input -> Int
solve2 = length . filter supportsSSL

test2 = ["aba[bab]xyz", "xyx[xyx]xyx", "aaa[kek]eke", "zazbz[bzb]cdb"]

puzzle1 = do
    s <- readFile "input07.txt"
    print (solve1 (parse s))

puzzle2 = do
    s <- readFile "input07.txt"
    print (solve2 (parse s))
