module SearchTree (SearchTree(..), unfoldTree, dfs, lfs) where

-- General search trees

-- Each child is paired with a non-negative cost increment.
data SearchTree a = Node a [(Int, SearchTree a)]

unfoldTree :: (a -> [(Int, a)]) -> a -> SearchTree a
unfoldTree f x = Node x [(d, unfoldTree f child) | (d, child) <- f x]

-- Two methods to find least cost of a success node

-- depth-first search, pruning branches that cost more than current best
dfs :: (a -> Bool) -> SearchTree a -> Int
dfs succeeded = search 0 maxBound
  where
    -- assume: sofar <= best
    search sofar best (Node x children)
      | sofar >= best = best::Int
      | succeeded x = sofar
      | otherwise =
        foldr id best [flip (search (sofar+n)) child | (n, child) <- children]

-- least-first search
lfs :: (a -> Bool) -> SearchTree a -> Int
lfs succeeded = search . singleton 0
  where
    search pq = case extract pq of
        Nothing -> error "no solution"
        Just (p, Node x children, pq')
          | succeeded x -> p
          | otherwise -> search (foldr add pq' children)
          where
            add (n, child) = insert (p+n) child

-- Lazy skew heaps, from "Fun with Binary Heap Trees" by Chris Okasaki
-- in "The Fun of Programming", pp 1-16.

data PQ a = Null | Fork !Int a (PQ a) (PQ a)

extract :: PQ a -> Maybe (Int, a, PQ a)
extract Null = Nothing
extract (Fork p x a b) = Just (p, x, merge a b)

empty :: PQ a
empty = Null

singleton :: Int -> a -> PQ a
singleton p x = Fork p x Null Null

insert :: Int -> a -> PQ a -> PQ a
insert p x a = merge (singleton p x) a

merge :: PQ a -> PQ a -> PQ a
merge a Null = a
merge Null b = b
merge a@(Fork ap ax aa ab) b@(Fork bp bx ba bb)
  | ap <= bp  = Fork ap ax ab (merge aa b)
  | otherwise = Fork bp bx bb (merge ba a)
