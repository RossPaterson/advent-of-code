-- | General-purpose utility functions
module Utilities (
    -- * Enumerated types
    allValues,
    succWrap,
    predWrap,
    -- * Lists
    paragraphs,
    readNumbers,
    takes,
    pairWith,
    fastNub,
    frequency,
    groupSortOn,
    mostCommon,
    initSets,
    leastBy,
    wordsWith,
    longZipWith,
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
    -- * Memoization
    memoize,
    -- * Rose trees
    -- | Functions for use with "Data.Tree"
    iterateTree,
    scanTree,
    takeTree,
    takeWhileTree,
    maximumDF,
    
    -- * Testing
    failures,
    ) where

import Data.Char
import Data.Function
import Data.List
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tree

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

-- | Extract positive numbers from a string
readNumbers :: String -> [Int]
readNumbers s
  | null start = []
  | otherwise = read number : readNumbers rest
  where
    start = dropWhile (not . isDigit) s
    (number, rest) = span isDigit start

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
frequency xs = Map.assocs (Map.fromListWith (+) [(x, 1) | x <- xs])

-- | Unique elements of the input list, in decreasing order of frequency
mostCommon :: Ord a => [a] -> [a]
mostCommon xs = map snd (sort [(-n, w) | (w, n) <- frequency xs])

-- | Order and group by value of @f@.
groupSortOn :: (Ord b) => (a -> b) -> [a] -> [[a]]
groupSortOn f = groupBy ((==) `on` f) . sortBy (compare `on` f)

-- | Faster equivalent of @map Set.fromList (inits xs)@.
-- Zipping this with the original list gives a way to find repetitions.
initSets :: Ord a => [a] -> [Set a]
initSets = scanl (flip Set.insert) Set.empty

-- | Maximal non-empty subsequences of elements satisfying @p@.
--
-- @words = wordsWith (not . isSpace)@
wordsWith :: (a -> Bool) -> [a] -> [[a]]
wordsWith p = unfoldr getWord
  where
    getWord xs = case dropWhile (not . p) xs of
        [] -> Nothing
        xs' -> Just (span p xs')

-- | Like 'zipWith', except that when one list is exhausted it keeps
-- going with the other one.
longZipWith :: (a -> a -> a) -> [a] -> [a] -> [a]
longZipWith _ [] ys = ys
longZipWith _ xs [] = xs
longZipWith f (x:xs) (y:ys) = f x y:longZipWith f xs ys

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
leastBy f = map fst . head . groupSortOn snd . map add_f
  where
    add_f x = (x, f x)

-- | Remove repeated elements from a list.
-- /O(n log(m))/ where m is the number of unique elements
fastNub :: Ord a => [a] -> [a]
fastNub = nub_aux Set.empty
  where
    nub_aux _ [] = []
    nub_aux seen (x:xs)
      | Set.member x seen = nub_aux seen xs
      | otherwise = x : nub_aux (Set.insert x seen) xs

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

-- | Memoize a recursive function over a given domain, with a default
-- value for arguments outside the domain.  If unsure that the domain
-- is correct, use an 'error' expression for the default.
--
-- @memoize domain def rec_f@ is a faster equivalent of
--
-- @
-- f x
--   | Set.member x domain = rec_f f x
--   | otherwise = def
-- @
--
-- If this function is to be applied to more than one argument, all the
-- applications should share the value of @memoize domain def rec_f@,
-- for example by defining
--
-- @
-- f = memoize domain def rec_f
-- @
--
-- with no class constraints in the type, so that all applications of @f@
-- share the same memo table.
memoize :: (Ord a) => Set a -> b -> ((a -> b) -> a -> b) -> a -> b
memoize domain def rec_f = f
  where
    f_table = Map.fromSet (rec_f f) domain
    f x = Map.findWithDefault def x f_table

-- Utilities on rose trees

-- | Build a tree.
iterateTree :: (a -> [a]) -> a -> Tree a
iterateTree f x = Node x (map (iterateTree f) (f x))

-- | Downwards accumulation with the specified binary operation.
scanTree :: (s -> a -> s) -> s -> Tree a -> Tree s
scanTree f s (Node x ts) = Node s' (map (scanTree f s') ts)
  where
    s' = f s x

-- | Prune a tree at depth @n@. The root is always included.
takeTree :: Int -> Tree a -> Tree a
takeTree n (Node x ts)
  | n == 0 = Node x []
  | otherwise = Node x (map (takeTree (n-1)) ts)

-- | The subtree including the root and all paths along while @p@ is 'True'.
takeWhileTree :: (a -> Bool) -> Tree a -> Tree a
takeWhileTree p (Node x ts) =
    Node x [takeWhileTree p t | t <- ts, p (rootLabel t)]

-- | Faster version of @'maximum' . 'fmap' score@ using a depth-first
-- traversal, provided @bound ('rootLabel' t) >= score x@ for any @x@ in @t@.
maximumDF :: (Ord v) => (a -> v) -> (a -> v) -> Tree a -> v
maximumDF score bound (Node root subts) = maximumForestDF (score root) subts
  where
    maximumForestDF = foldl maximumTreeDF

    maximumTreeDF best (Node x ts)
      | bound x <= best = best
      | otherwise = maximumForestDF (max best (score x)) ts

-- | Run a function on a number of test inputs with expected outputs
-- and report any mismatches.
failures :: (Show a, Show b, Eq b) =>
    String -> (a -> b) -> [(a, b)] -> [String]
failures fname f xys =
    [fname ++ " " ++ showsPrec 11 x "" ++ " = " ++ show (f x) ++
        " (expected " ++ show y ++ ")" | (x, y) <- xys, f x /= y]
