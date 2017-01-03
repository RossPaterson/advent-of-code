module Utilities where

import Data.List

-- select all the elements of xs that have the least value of f
leastBy :: (Ord v) => (a -> v) -> [a] -> [a]
leastBy f = head . groupBy same . sortOn f
  where
    same x y = f x == f y
