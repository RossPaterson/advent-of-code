module Day11 where

import Utilities
import Data.Bits
import Data.List
import Data.Maybe
import Data.Word
import Data.Set (Set)
import qualified Data.Set as Set
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Graph.AStar

type Element = String

data Object e = Gen e | Chip e
  deriving (Show, Eq, Ord)
type Floor e = [Object e]
data State e = State { elevator :: Int, contents :: [Floor e] }
  deriving (Show, Eq, Ord)
type ExtState = State String

instance Functor Object where
    fmap f (Gen e) = Gen (f e)
    fmap f (Chip e) = Chip (f e)

-- Replace element names with 0..

normalize :: ExtState -> State Int
normalize (State elev objects) = State elev objects'
  where
    objects' = map (map (fmap replace)) objects
    replace e = fromJust (elemIndex e es)
    es = elements (State elev objects)

elements :: Ord e => State e -> [e]
elements =
    Set.toList . Set.unions . map (Set.fromList . map element) . contents

element :: Object e -> e
element (Gen e) = e
element (Chip e) = e

-- testing of floors

generators :: Ord e => Floor e -> [e]
generators objects = sort [e | Gen e <- objects]

microchips :: Ord e => Floor e -> [e]
microchips objects = sort [e | Chip e <- objects]

safeState :: State Int -> Bool
safeState = all safeFloor . contents

safeFloor :: Floor Int -> Bool
safeFloor objects = null gens || null (chips \\ gens)
  where
    gens = generators objects
    chips = microchips objects

-- finished if all objects on top floor
finished :: State Int -> Bool
finished = all null . init . contents

-- lower bound on number of steps to finish
heuristic :: State Int -> Int
heuristic (State e objects) =
    -- sum [n*length os | (n, os) <- zip [0..] (reverse objects)] - 1
    sum [max 1 (2*n-3) | n <- scanl1 (+) (map length (init objects))]

-- possible moves from state

moves :: State Int -> [State Int]
moves s = move_up s ++ move_down s

move_up :: State Int -> [State Int]
move_up (State e objects)
  | e >= length objects = []
  | otherwise = [State e' (below ++ this' : next' : above) | (this', next') <- simpleMoves this next]
  where
    e' = e+1
    (below, this:next:above) = splitAt (e-1) objects

move_down :: State Int -> [State Int]
move_down (State e objects)
  | e == 1 = []
  | otherwise = [State e' (below ++ next' : this' : above) | (this', next') <- simpleMoves this next]
  where
    e' = e-1
    (below, next:this:above) = splitAt (e-2) objects

simpleMoves :: Floor Int -> Floor Int -> [(Floor Int, Floor Int)]
simpleMoves src dest =
    [(src', dest') |
        n <- [1,2], (picks, src') <- choose n src,
        safeFloor src',
        let dest' = picks ++ dest,
        safeFloor dest']

-- Compact representation of state:
-- bits 0-7: number of elements
-- bits 8-9: position of elevator (F1 = 0)
-- bits 10-11: position of E1 generator
-- bits 12-13: position of E1 microchip
type IntState = Word64

encodeState :: State Int -> Word64
encodeState s@(State elev objects) =
    fromIntegral n .|.
    (fromIntegral (elev-1) `shiftL` 4) .|.
    orAll (map orAll (zipWith (map . encode) [0..] objects))
  where
    orAll = foldr (.|.) 0
    n = length (elements s)
    encode f (Gen e) = fromIntegral f `shiftL` (4*e + 6)
    encode f (Chip e) = fromIntegral f `shiftL` (4*e + 8)

decodeState :: Word64 -> State Int
decodeState bits = State elev objects
  where
    n = fromIntegral (bits .&. 0xf)
    elev = fromIntegral ((bits `shiftR` 4) .&. 0x3) + 1
    objects = [decodeFloor f | f <- [0..3]]
    decodeFloor f = concat [decode f e | e <- [0..n-1]]
    decode f e =
        (if fromIntegral (slice .&. 0x3) == f
            then (Gen e:) else id) $
        (if fromIntegral ((slice `shiftR` 2) .&. 0x3) == f
            then (Chip e:) else id) $
        []
      where
        slice = bits `shiftR` (4*e + 6)

solve :: ExtState -> Int
solve = length . fromJust .
    aStar (HashSet.fromList . map encodeState . moves . decodeState)
          (const (const 1))
          (heuristic . decodeState)
          (finished . decodeState) .
    encodeState . normalize

testState :: ExtState
testState = State 1 [
    -- The first floor contains a hydrogen-compatible microchip and
    -- a lithium-compatible microchip.
    [Chip "H", Chip "L"],
    -- The second floor contains a hydrogen generator.
    [Gen "H"],
    -- The third floor contains a lithium generator.
    [Gen "L"],
    -- The fourth floor contains nothing relevant.
    []]

initState :: ExtState
initState = State 1 [
    -- The first floor contains a thulium generator, a thulium-compatible
    -- microchip, a plutonium generator, and a strontium generator.
    [Gen "Th", Chip "Th", Gen "Pu", Gen "Sr"],
    -- The second floor contains a plutonium-compatible microchip and a
    --  strontium-compatible microchip.
    [Chip "Pu", Chip "Sr"],
    -- The third floor contains a promethium generator, 
    -- a promethium-compatible microchip, a ruthenium generator, and 
    -- a ruthenium-compatible microchip.
    [Gen "Pm", Chip "Pm", Gen "Ru", Chip "Ru"],
    -- The fourth floor contains nothing relevant.
    []]

puzzle1 :: IO ()
puzzle1 = print (solve initState)

initState2 :: ExtState
initState2 = State 1 ((extras++floor1):rest)
  where
    floor1:rest = contents initState
    -- An elerium generator.
    -- An elerium-compatible microchip.
    -- A dilithium generator.
    -- A dilithium-compatible microchip
    extras = [Gen "EL", Chip "EL", Gen "DL", Chip "DL"]

puzzle2 :: IO ()
puzzle2 = print (solve initState2)
