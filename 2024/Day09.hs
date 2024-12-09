module Main where

import Utilities
import Data.Char

-- Input processing

type Input = [Digit]
type Digit = Int

parse :: String -> Input
parse = map digitToInt . filter isDigit

-- Part One

-- start (counting from 0) and length
data Chunk = Chunk Int Int
    deriving (Show)

-- file ID and extent
data File = File Int Chunk
    deriving (Show)

-- The key point is that we process files from the right and gaps from
-- the left, stopping when we cross over.  Therefore, we make them two
-- separate lists, and will reverse the list of files before processing.

-- interpret numbers as files and free space
chunks :: [Int] -> ([File], [Chunk])
chunks ns = (files, gaps)
  where
    files = zipWith File [0..] (odds loc_ns)
    gaps = evens loc_ns
    loc_ns = zipWith Chunk (scanl (+) 0 ns) ns

-- elements in odd positions (counting from 1)
odds :: [a] -> [a]
odds [] = []
odds (x:xs) = x:evens xs

-- elements in even positions (counting from 1)
evens :: [a] -> [a]
evens [] = []
evens (_:xs) = odds xs

solve1 :: Input -> Int
solve1 = checksum . moveBlocks . chunks

-- move blocks of files into gaps from the left
moveBlocks :: ([File], [Chunk]) -> [File]
moveBlocks (fs, gaps) = moveBlocksAux (reverse fs) gaps

moveBlocksAux :: [File] -> [Chunk] -> [File]
moveBlocksAux all_fs@(File label (Chunk fstart flen):fs)
    (gc@(Chunk gstart glen):gaps)
  | fstart < gstart = all_fs
  | flen == glen = -- fills the gap exactly
    File label (Chunk gstart flen) : moveBlocksAux fs gaps
  | flen < glen = -- fits in the gap with some space left
    File label (Chunk gstart flen) : moveBlocksAux fs (takeSpace flen gc:gaps)
  | otherwise = -- part of it fills the gap
    File label (Chunk gstart glen) :
        moveBlocksAux (File label (Chunk fstart (flen-glen)):fs) gaps
moveBlocksAux fs _ = fs

-- take the front part of the gap (assumes n < len)
takeSpace :: Int -> Chunk -> Chunk
takeSpace n (Chunk start len) = Chunk (start+n) (len-n)

checksum :: [File] -> Int
checksum fs = sum [n*chunkSum c | File n c <- fs]

-- sum [start..start+n-1]
chunkSum :: Chunk -> Int
chunkSum (Chunk start n) = start*n + n*(n-1) `div` 2

testInput :: String
testInput = "\
    \2333133121414131402\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 1928)]

-- Part Two

solve2 :: Input -> Int
solve2 = checksum . moveFiles . chunks

-- move whole files into gaps from the left
moveFiles :: ([File], [Chunk]) -> [File]
moveFiles (fs, gaps) = moveFilesAux (reverse fs) gaps

moveFilesAux :: [File] -> [Chunk] -> [File]
moveFilesAux [] _ = []
moveFilesAux fs [] = fs
moveFilesAux (f@(File label (Chunk fstart flen)):fs) gaps =
    case tryShift flen gapsOnLeft of
        Nothing -> f : moveFilesAux fs gapsOnLeft
        Just (gstart, gaps') ->
            File label (Chunk gstart flen) : moveFilesAux fs gaps'
  where
    gapsOnLeft = takeWhile onLeft gaps
    onLeft (Chunk gstart _) = gstart < fstart

-- try to shift a chunk of the given size into the first of the gaps that
-- it fits, returning the new start of the chunk, plus the remaining gaps
tryShift :: Int -> [Chunk] -> Maybe (Int, [Chunk])
tryShift flen gaps =
    case span tooSmall gaps of
        (_, []) -> Nothing
        (front, gc@(Chunk gstart glen):back)
          | flen == glen -> Just (gstart, front ++ back)
          | otherwise -> Just (gstart, front ++ takeSpace flen gc:back)
  where
    tooSmall (Chunk _ len) = len < flen

tests2 :: [(String, Int)]
tests2 = [(testInput, 2858)]

main :: IO ()
main = do
    s <- readFile "input/09.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
