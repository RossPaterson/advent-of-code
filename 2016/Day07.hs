module Main where

import Utilities
import Parser
import Control.Applicative
import Data.List
import Data.Maybe

data Address = Address String [(String, String)]
  deriving Show
type Input = [Address]

parse :: String -> Input
parse = map (runParser address) . lines
  where
    address = Address <$> word <*> many ((,) <$> hyperword <*> word)
    hyperword = char '[' *> word <* char ']'
    word = many (satisfy (\ c -> c /= '[' && c /= ']'))

supernet :: Address -> [String]
supernet (Address w hws) = w : map snd hws

hypernet :: Address -> [String]
hypernet (Address _ hws) = map fst hws

supportsTLS :: Address -> Bool
supportsTLS addr =
    any hasABBA (supernet addr) && not (any hasABBA (hypernet addr))

hasABBA :: String -> Bool
hasABBA = any isABBA . tails

isABBA :: String -> Bool
isABBA (c1:c2:c3:c4:_) = c1 == c4 && c2 == c3 && c1 /= c2
isABBA _ = False

solve1 :: Input -> Int
solve1 = length . filter supportsTLS

tests1 :: [(String, Bool)]
tests1 = [
    ("abba[mnop]qrst", True),
    ("abcd[bddb]xyyx", False),
    ("aaaa[qwer]tyui", False),
    ("ioxxoj[asdfgh]zxcvbn", True)]

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

tests2 :: [(String, Bool)]
tests2 = [
    ("aba[bab]xyz", True),
    ("xyx[xyx]xyx", False),
    ("aaa[kek]eke", True),
    ("zazbz[bzb]cdb", True)]

main :: IO ()
main = do
    s <- readFile "input/07.txt"
    let input = parse s
    putStr (unlines (failures "supportsTLS" (supportsTLS . head . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "supportsSSL" (supportsSSL . head . parse) tests2))
    print (solve2 input)
