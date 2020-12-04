module Main where

import Utilities
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

-- Input processing

type Input = [Record]
type Record = [(String, String)]

parse :: String -> Input
parse = map (map mkPair . words) . paragraphs
  where
    mkPair s = case span (/= ':') s of
        (f, ':':v) -> (f, v)
        _ -> error "missing colon"

-- Part One

fields :: [String]
fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"]

-- select the fields to be checked, in alhabetical order
normalize :: [String] -> [String]
normalize fs = sort (delete "cid" fs)

requiredFields :: [String]
requiredFields = normalize fields

-- Ignoring "cid", the record contains the required fields and no more.
valid1 :: Record -> Bool
valid1 fs = normalize (map fst fs) == requiredFields

solve1 :: Input -> Int
solve1 = length . filter valid1

testInput1 :: String
testInput1 = "\
    \ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\n\
    \byr:1937 iyr:2017 cid:147 hgt:183cm\n\
    \\n\
    \iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\n\
    \hcl:#cfa07d byr:1929\n\
    \\n\
    \hcl:#ae17e1 iyr:2013\n\
    \eyr:2024\n\
    \ecl:brn pid:760753108 byr:1931\n\
    \hgt:179cm\n\
    \\n\
    \hcl:#cfa07d eyr:2025 pid:166559648\n\
    \iyr:2011 ecl:brn hgt:59in\n"

tests1 :: [(String, Int)]
tests1 = [(testInput1, 2)]

-- Part Two

-- checker for a field value
type Validation = String -> Bool

validation :: Map String Validation
validation = Map.fromList [
    ("byr", yearBetween 1920 2002),
    ("iyr", yearBetween 2010 2020),
    ("eyr", yearBetween 2020 2030),
    ("hgt", height),
    ("hcl", haircolor),
    ("ecl", flip elem ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]),
    ("pid", passportID)]

yearBetween :: Int -> Int -> Validation
yearBetween lo hi s = length s == 4 && all isDigit s && lo <= n && n <= hi
  where
    n = read s

height :: Validation
height s = front /= "" &&
    (back == "cm" && 150 <= n && n <= 193) ||
    (back == "in" && 59 <= n && n <= 76)
  where
    (front, back) = span isDigit s
    n = read front::Int

haircolor :: Validation
haircolor ('#':s) = length s == 6 && all isHex s
haircolor _ = False

isHex :: Char -> Bool
isHex c = '0' <= c && c <= '9' || 'a' <= c && c <= 'f'

passportID :: Validation
passportID s = length s == 9 && all isDigit s

valid2 :: Record -> Bool
valid2 fs = valid1 fs &&
    and (Map.intersectionWith ($) validation (Map.fromList fs))

solve2 :: Input -> Int
solve2 = length . filter valid2

testInput2 :: String
testInput2 = "\
    \eyr:1972 cid:100\n\
    \hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926\n\
    \\n\
    \iyr:2019\n\
    \hcl:#602927 eyr:1967 hgt:170cm\n\
    \ecl:grn pid:012533040 byr:1946\n\
    \\n\
    \hcl:dab227 iyr:2012\n\
    \ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277\n\
    \\n\
    \hgt:59cm ecl:zzz\n\
    \eyr:2038 hcl:74454a iyr:2023\n\
    \pid:3556412378 byr:2007\n"

testInput3 :: String
testInput3 = "\
    \pid:087499704 hgt:74in ecl:grn iyr:2012 eyr:2030 byr:1980\n\
    \hcl:#623a2f\n\
    \\n\
    \eyr:2029 ecl:blu cid:129 byr:1989\n\
    \iyr:2014 pid:896056539 hcl:#a97842 hgt:165cm\n\
    \\n\
    \hcl:#888785\n\
    \hgt:164cm byr:2001 iyr:2015 cid:88\n\
    \pid:545766238 ecl:hzl\n\
    \eyr:2022\n\
    \\n\
    \iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719\n"

tests2 :: [(String, Int)]
tests2 = [(testInput2, 0), (testInput3, 4)]

main :: IO ()
main = do
    s <- readFile "input/04.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
