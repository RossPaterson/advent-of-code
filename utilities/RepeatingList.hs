-- a sequence that is either finite or repeats from some point
module RepeatingList where

import Utilities
import Data.Foldable
import Data.Map (Map, (!))
import qualified Data.Map as Map
import Data.Sequence (Seq, ViewL(..), ViewR(..), (<|), (|>), (><))
import qualified Data.Sequence as Seq
import Prelude hiding (iterate, lookup)

data RepeatingList a =
    RepeatingList { front :: !(Seq a), recurring :: !(Seq a) }
  deriving Show

instance Functor RepeatingList where
    fmap f (RepeatingList fr re) = RepeatingList (fmap f fr) (fmap f re)

instance Foldable RepeatingList where
    foldr f z (RepeatingList fr re) = foldr f rest fr
      where
        rest
          | Seq.null re = z
          | otherwise = foldr f rest re

-- finite list
fromList :: [a] -> RepeatingList a
fromList xs = RepeatingList (Seq.fromList xs) Seq.empty

iterate :: Ord a => (a -> a) -> a -> RepeatingList a
iterate f = loop Seq.empty Map.empty
  where
    loop prefix position x = case Map.lookup x position of
        Nothing ->
            loop (prefix |> x) (Map.insert x (Seq.length prefix) position) (f x)
        Just pos -> RepeatingList fr re
          where
            (fr, re) = Seq.splitAt pos prefix

-- lookup n = Data.List.lookup n . toList, but faster
lookup :: Int -> RepeatingList a -> Maybe a
lookup n (RepeatingList fr re)
  | n <= Seq.length fr = Seq.lookup n fr
  | Seq.null re = Nothing
  | otherwise = Seq.lookup ((n - Seq.length fr) `mod` Seq.length re) re

-- mconcatTake n = mconcat . take n . toList, but faster
mconcatTake :: Monoid a => Int -> RepeatingList a -> a
mconcatTake n (RepeatingList fr re)
  | n <= Seq.length fr || Seq.null re = fold (Seq.take n fr)
  | otherwise = fold fr <> mtimes q (fold re) <> fold (Seq.take r re)
  where
    (q, r) = (n - Seq.length fr) `divMod` Seq.length re
