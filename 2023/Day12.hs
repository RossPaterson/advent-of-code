module Main where

import Parser
import Utilities
import Control.Applicative
import Data.List
import qualified Data.Set as Set

-- Input processing

type Input = [Record]
type Record = (Pattern, [Int])
type Pattern = [Maybe Condition]
data Condition = Operational | Damaged
    deriving (Eq, Show)

parse :: String -> Input
parse = map (runParser cond) . lines
  where
    cond = (,) <$> pattern <* space <*> numbers
    pattern = some patt_char
    patt_char =
        Nothing <$ char '?' <|>
        Just Damaged <$ char '#' <|>
        Just Operational <$ char '.'
    numbers = nat `sepBy1` char ','

-- Part One

-- the number of consistent arrangements, using exhaustive search
arrangements_simple :: Record -> Int
arrangements_simple (cs, gs) =
    length [bs | bs <- subs cs, groups bs == gs]

-- all possible substitutions for unknowns
subs :: Pattern -> [[Condition]]
subs = traverse (maybe [Operational, Damaged] (:[]))

-- sizes of groups of adjacent damaged springs
groups :: [Condition] -> [Int]
groups bs = [length g | g <- group bs, head g == Damaged]

solve1 :: Input -> Int
solve1 = sum . map arrangements_simple

testInput :: String
testInput = "\
    \???.### 1,1,3\n\
    \.??..??...?##. 1,1,3\n\
    \?#?#?#?#?#?#?#? 1,3,1,6\n\
    \????.#...#... 4,1,1\n\
    \????.######..#####. 1,6,5\n\
    \?###???????? 3,2,1\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 21)]

-- Part Two

-- Block of adjacent damaged and unknown elements, the only places
-- where groups of adjacent damaged elements can occur.
-- True if unknown, False if damaged.
type Block = [Bool]

-- The blocks of a pattern.  The number of operational elements separating
-- them is immaterial for this problem.
blocks :: Pattern -> [Block]
blocks = map (map (== Nothing)) . wordsWith (/= Just Operational)

-- An unwrapped recursive definition of a function (so it can be memoized)
type Recursive a b = (a -> b) -> a -> b

-- Using indices instead of lists in the keys would have sped things up.

-- unwrapped function to count matches of a single block against group sizes
matchBlockRec :: Recursive (Block, [Int]) Int
-- if there are no elements, there must be no groups
matchBlockRec _ ([], []) = 1
matchBlockRec _ ([], _:_) = 0
-- if the first element is damaged, the first group must start here
matchBlockRec mb (False:ps, ns) = matchBlockStart mb ps ns
-- if the first element is unknown, the first group could start here or later
matchBlockRec mb (True:ps, ns) = matchBlockStart mb ps ns + mb (ps, ns)

-- Auxilliary function counting matches if which the first group occurs
-- at the start of the block, the first element of which has been removed.
matchBlockStart :: ((Block, [Int]) -> Int) -> Block -> [Int] -> Int
matchBlockStart _ _ [] = 0
matchBlockStart _ ps [n]
  | length ps >= n-1 && and (drop (n-1) ps) = 1
  | otherwise = 0
matchBlockStart mb ps (n:ns) =
    case drop (n-1) ps of
    (True:ps') -> mb (ps', ns)
    _ -> 0

-- unwrapped function to count matches of a list of blocks against group sizes
matchBlocksRec ::
    ((Block, [Int]) -> Int) -> Recursive ([Block], [Int]) Int
matchBlocksRec _ _ ([], []) = 1
matchBlocksRec _ _ ([], _:_) = 0
matchBlocksRec mb mbs (b:bs, ns) =
    sum [mb (b, ns1)*mbs (bs, ns2) | (ns1, ns2) <- splits ns]

-- number of matches of a list of blocks against group sizes
matchBlocks :: [Block] -> [Int] -> Int
matchBlocks bs ns =
    memoize mbs_args 0 (matchBlocksRec matchBlock) (bs, ns)
  where
    mbs_args =
        Set.fromList [(bs', ns') |
            bs' <- tails bs, ns' <- tails ns,
            length_separated (map length bs') >= length_separated ns']
    matchBlock = memoize mb_args 0 matchBlockRec
    mb_args =
        Set.fromList [(b', ns') |
            b <- bs, b' <- tails b, ns' <- segments ns,
            length b' >= length_separated ns']

-- sum of lengths with 1 added between pairs of elements
length_separated :: [Int] -> Int
length_separated ns = sum ns + max 0 (length ns - 1)

-- contiguous sublists
segments :: [a] -> [[a]]
segments xs = [] : [zs | ys <- tail (inits xs), zs <- init (tails ys)]

-- the number of consistent arrangements, using dynamic programming
-- (memoization)
arrangements :: Record -> Int
arrangements (cs, gs) = matchBlocks (blocks cs) gs

-- n copies, separated by unknowns
unfold :: Int -> Record -> Record
unfold n (cs, gs) =
    (intercalate [Nothing] (replicate n cs), concat (replicate n gs))

solve2 :: Input -> Int
solve2 = sum . map arrangements . map (unfold 5)

tests2 :: [(String, Int)]
tests2 = [(testInput, 525152)]

main :: IO ()
main = do
    s <- readFile "input/12.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
