module Main where

import Utilities
import Intcode
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq, ViewL(..), (|>))
import qualified Data.Sequence as Seq

-- Input processing

type Input = Memory

parse :: String -> Input
parse = readMemory

-- Part One

data Node = Node Automaton (Seq Packet)
type Packet = (Integer, Integer)
type Network = Map Int Node

initNetwork :: Int -> Memory -> Network
initNetwork n mem = Map.fromList [(i, initNode i mem) | i <- [0..n-1]]

initNode :: Int -> Memory -> Node
initNode n mem = case automaton mem of
    ReadValue k -> Node (k (toValue n)) Seq.empty
    _ -> error "node not initially accepting input"

data State = State {
    curr_node :: Int,
    result :: Maybe Packet,
    nodes :: Network
    }

initState :: Int -> Memory -> State
initState n mem = State {
    curr_node = 0,
    result = Nothing,
    nodes = initNetwork n mem
    }

-- do input or output on current node
step :: State -> State
step (State n res net) = case Map.lookup n net of
    Nothing -> error $ "no computer " ++ show n
    Just (Node mc q) -> case mc of
        ReadValue k -> State n res (Map.insert n (readValue k q) net)
        WriteValue dest (WriteValue x (WriteValue y mc')) ->
            writeValue (fromValue dest) (x, y) (Map.insert n (Node mc' q) net)
        _ -> error "other state"
  where
    readValue k q = case Seq.viewl q of
        EmptyL -> Node (k (-1)) q
        (x, y) :< q' -> case k x of
            ReadValue kx -> Node (kx y) q'
            _ -> error "node not accepting Y"

    writeValue dest p net'
      | dest == 255 = State n (Just p) net'
      | otherwise = State n res (sendPacket dest p net')

sendPacket :: Int -> Packet -> Network -> Network
sendPacket dest p net = Map.adjust (receivePacket p) dest net

receivePacket :: Packet -> Node -> Node
receivePacket p (Node mc q) = Node mc (q |> p)

-- move to the next node
advance :: State -> State
advance s = s { curr_node = (curr_node s + 1) `mod` 50 }

solve1 :: Input -> Int
solve1 =
    fromValue . snd . head . catMaybes . map result .
        iterate (advance . step) . initState 50

-- Part Two

-- node is reading from an empty queue
idleNode :: Node -> Bool
idleNode (Node (ReadValue _) q)
  | Seq.null q = True
idleNode _ = False

-- send packet to restart network if idle
monitor :: State -> (Maybe Packet, State)
monitor (State n res net)
  | all idleNode net = case res of
    Nothing -> error "nothing saved"
    Just p -> (Just p, State n res (sendPacket 0 p net))
monitor s = (Nothing, s)

-- immediately repeated values in the list
dups :: Eq a => [a] -> [a]
dups xs = [x1 | (x1, x2) <- zip xs (tail xs), x1 == x2]

solve2 :: Input -> Int
solve2 =
    fromValue . head . dups . map snd .
        catMaybes . unfoldr (Just . monitor . times 50 (advance . step)) .
        initState 50

main :: IO ()
main = do
    s <- readFile "input/23.txt"
    let input = parse s
    print (solve1 input)
    print (solve2 input)
