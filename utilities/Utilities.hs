-- | General-purpose utility functions
module Utilities (
    -- * Enumerated types
    allValues,
    succWrap,
    predWrap,
    -- * Lists
    paragraphs,
    takes,
    pairWith,
    fastNub,
    frequency,
    mostCommon,
    leastBy,
    same,
    tsort,
    tsortG,
    -- * Iteration
    whileJust,
    iterateWhileJust,
    whileRight,
    iterateWhileRight,
    convergeBy,
    times,
    -- * Searching
    splits,
    choose,
    chooseBetween,
    bsearch,
    -- * Testing
    failures,
    ) where

import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Ord
import qualified Data.Set as Set

-- | All the values of a bounded enumerated type
allValues :: (Bounded a, Enum a) => [a]
allValues = [minBound..maxBound]

-- | Like 'succ', but wraps at 'maxBound'
succWrap :: (Eq a, Bounded a, Enum a) => a -> a
succWrap v
  | v == maxBound = minBound
  | otherwise = succ v

-- | Like 'pred', but wraps at 'minBound'
predWrap :: (Eq a, Bounded a, Enum a) => a -> a
predWrap v
  | v == minBound = maxBound
  | otherwise = pred v

-- | Break a string into chunks separated by blank lines (two consecutive
-- newlines).  Each chunk in the result ends with a newline.
paragraphs :: String -> [String]
paragraphs "" = []
paragraphs "\n" = []
paragraphs ('\n':'\n':cs) = "\n":paragraphs cs
paragraphs (c:cs) = case paragraphs cs of
    [] -> [[c, '\n']]
    p:ps -> (c:p):ps

-- | @'takes' n xs@ partitions @xs@ into groups of size @n@, with the
-- possible exception of the last one, which is non-empty.
takes :: Int -> [a] -> [[a]]
takes n = takeWhile (not . null) . map (take n) . iterate (drop n)

-- | Combine adjacent pairs, halving the size of the list
pairWith :: (a -> a -> a) -> [a] -> [a]
pairWith f (x1:x2:xs) = f x1 x2:pairWith f xs
pairWith _ xs = xs

-- | Unique elements of the input list, paired with their number of occurrences
frequency :: Ord a => [a] -> [(a, Int)]
frequency xs = [(head g, length g) | g <- group (sort xs)]

-- | Unique elements of the input list, in decreasing order of frequency
mostCommon :: Ord a => [a] -> [a]
mostCommon xs = map snd (sort [(-n, w) | (w, n) <- frequency xs])

-- | Repeatedly apply the function until it doesn't produce a new value
whileJust :: (a -> Maybe a) -> a -> a
whileJust f x = maybe x (whileJust f) (f x)

-- | Repeatedly apply the function until it doesn't produce a new value,
-- collecting all the values
iterateWhileJust :: (a -> Maybe a) -> a -> [a]
iterateWhileJust f x = x:maybe [] (iterateWhileJust f) (f x)

-- | Repeatedly apply the function until it produces a 'Left' value
whileRight :: (a -> Either b a) -> a -> b
whileRight f x = either id (whileRight f) (f x)

-- | Repeatedly apply the function until it produces a 'Left' value,
-- collecting all the 'Right' values
iterateWhileRight :: (a -> Either b a) -> a -> [a]
iterateWhileRight f x = x : either (const []) (iterateWhileRight f) (f x)

-- | @'convergeBy' p xs@ returns the first element of @xs@ related to
-- the previous one by @p@.
convergeBy :: (a -> a -> Bool) -> [a] -> a
convergeBy p xs = head [x2 | (x1, x2) <- zip xs (tail xs), p x1 x2]

-- | Apply a function @n@ times
times :: Int -> (a -> a) -> a -> a
times n f = compose (replicate n f)

-- | Composition of a list of functions
compose :: [a -> a] -> a -> a
compose fs x = foldr id x fs

-- | Ways of splitting a list, earliest first
splits :: [a] -> [([a], [a])]
splits xs = zip (inits xs) (tails xs)

-- | Possible choices of n elements from a list.  Elements of each list
-- have the same relative ordering as in the input list.
choose :: Int -> [a] -> [([a], [a])]
choose 0 xs = [([], xs)]
choose _ [] = []
choose n (x:xs) =
    [(x:ys, rest) | (ys, rest) <- choose (n-1) xs] ++
    [(ys, x:rest) | (ys, rest) <- choose n xs]

-- | Possible choices of between m and n items (m <= n)
chooseBetween :: Int -> Int -> [a] -> [[a]]
chooseBetween m _ [] = [[] | m == 0]
chooseBetween m n (x:xs) =
    [x:ys | n > 0,
        ys <- chooseBetween (max 0 (m-1)) (n-1) xs] ++ chooseBetween m n xs

-- | Select all the elements of xs that have the least value of f
-- (f is evaluated once for each element of the list.)
leastBy :: (Ord v) => (a -> v) -> [a] -> [a]
leastBy f =
    map fst . head . groupBy (same snd) . sortBy (comparing snd) . map add_f
  where
    add_f x = (x, f x)

-- | Equal results under f (useful for 'groupBy')
same :: (Eq b) => (a -> b) -> a -> a -> Bool
same f x y = f x == f y

-- | Sort and eliminate repetitions.
-- /O(n log(m))/ where m is the number of unique elements
fastNub :: Ord a => [a] -> [a]
fastNub = Set.toList . Set.fromList

-- | Topological sort
tsort :: Ord a => [(a, a)] -> [a]
tsort = tsortG . relationToGraph

-- | Topological sort
tsortG :: Ord a => Map a [a] -> [a]
tsortG g = map fst $ sortBy (comparing (Down . snd)) $ Map.assocs depth_map
  where
    depth_map = fmap depth (Map.union g empties)
    empties = Map.unions [Map.singleton x [] | xs <- Map.elems g, x <- xs]
    depth [] = 0::Int
    depth ys = maximum [depth_map!y | y <- ys] + 1

relationToGraph :: Ord a => [(a, a)] -> Map a [a]
relationToGraph xys =
    Map.map Set.toList $ Map.unionsWith Set.union $
        [Map.singleton x (Set.singleton y) | (x, y) <- xys]

-- | For a non-constant upward-closed predicate @p@, @'bsearch' p@ returns
-- the least @n@ satisfying @p@, using /O(log n)/ evaluations of @p@.
--
-- If @p@ is constant, no such @n@ exists and @'bsearch' p@ does not
-- terminate.
bsearch :: (Integer -> Bool) -> Integer
bsearch p
  | p 0 = negate (searchFrom (not . p . negate) 0) + 1
  | otherwise = searchFrom p 1

searchFrom :: (Integer -> Bool) -> Integer -> Integer
searchFrom p = search_from 1
  where
    search_from step l
      | p l' = searchIntegerRange p l (l'-1)
      | otherwise = search_from (2*step) (l'+1)
      where l' = l + step

searchIntegerRange :: (Integer -> Bool) -> Integer -> Integer -> Integer
searchIntegerRange p l h
  | h < l = h+1
  | p m = searchIntegerRange p l (m-1)
  | otherwise = searchIntegerRange p (m+1) h
  where m = (l+h) `div` 2

-- | Run a function on a number of test inputs with expected outputs
-- and report any mismatches.
failures :: (Show a, Show b, Eq b) =>
    String -> (a -> b) -> [(a, b)] -> [String]
failures fname f xys =
    [fname ++ " " ++ showsPrec 11 x "" ++ " = " ++ show (f x) ++
        " (expected " ++ show y ++ ")" | (x, y) <- xys, f x /= y]
