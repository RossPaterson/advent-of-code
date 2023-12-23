module Main where

import Parser
import Utilities
import Control.Applicative
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Sequence (Seq, (|>), ViewL(..))
import qualified Data.Sequence as Seq

-- Input processing

type Wiring = Map ModuleName Output
type Output = (Type, [ModuleName])
data Type = Broadcaster | FlipFlop | Conjunction
    deriving (Eq, Show)
type ModuleName = String

rootModule :: ModuleName
rootModule = "broadcaster"

type Input = Wiring

parse :: String -> Input
parse = Map.fromList . map (runParser wiring) . lines
  where
    wiring = assoc <$> source <* string " -> " <*> destinations
    source =
        ((,) Broadcaster) <$> string rootModule <|>
        ((,) FlipFlop) <$ char '%' <*> name <|>
        ((,) Conjunction) <$ char '&' <*> name
    destinations = sepBy1 name (string ", ")
    assoc (ty, src) dests = (src, (ty, dests))
    name = (:) <$> letter <*> many (letter <|> digit)

-- Part One

data State = State {
    -- whether each FlipFlop is On
    flipflops :: Map ModuleName Bool,
    -- remembered inputs of each Conjunction
    conjunctions :: Map ModuleName (Map ModuleName Bool),
    -- value of untyped destination modules
    untyped :: Map ModuleName Bool,
    lows :: !Int, -- number of low pulses transmitted
    highs :: !Int -- number of high pulses transmitted
    }
    deriving (Show)

initState :: Wiring -> State
initState m = State {
    flipflops = Map.fromSet (const False) flipflops_set,
    conjunctions = Map.restrictKeys all_srcs conjunctions_set,
    untyped = Map.fromSet (const False) untyped_set,
    lows = 0,
    highs = 0
    }
  where
    -- sources for each destination
    all_srcs = Map.fromListWith Map.union
        [(dest, Map.singleton src False) |
            (src, (_, dests)) <- Map.assocs m,
            dest <- dests]
    flipflops_set =
        Set.fromList [src | (src, (FlipFlop, _)) <- Map.assocs m]
    conjunctions_set =
        Set.fromList [src | (src, (Conjunction, _)) <- Map.assocs m]
    untyped_set =
        Set.difference all_dests (Map.keysSet m)
    all_dests =
        Set.fromList [dest | (_, dests) <- Map.elems m, dest <- dests]

-- signal from a source to a destination
-- True = On = High
type Pulse = (ModuleName, Bool, ModuleName)
type PulseQueue = Seq Pulse

-- initial queue of pulses, consisting of single low pulse from the
-- button to the broadcaster
initPulseQueue :: PulseQueue
initPulseQueue = Seq.singleton ("button", False, rootModule)

-- pulse sending the same value from a source to multiple destinations
pulses :: ModuleName -> Bool -> [ModuleName] -> [Pulse]
pulses src p dests = [(src, p, dest) | dest <- dests]

-- Receive a pulse at its destination, updating the state and possibly
-- generating new pulses.
receive :: Wiring -> Pulse -> State -> (State, [Pulse])
receive wm pulse s = receiveAux wm pulse (countPulse pulse s)

receiveAux :: Wiring -> Pulse -> State -> (State, [Pulse])
receiveAux wm (src, p, dest) s =
    case Map.lookup dest wm of
    Nothing ->
        (s { untyped = Map.insert dest p (untyped s) }, [])
    Just (FlipFlop, dests)
      | p -> (s, [])
      | otherwise ->
        let v = not (flipflops s!dest) in
        (s { flipflops = Map.insert dest v (flipflops s) },
         pulses dest v dests)
    Just (Conjunction, dests) ->
        let m = Map.insert src p (conjunctions s!dest) in
        (s { conjunctions = Map.insert dest m (conjunctions s) },
         pulses dest (not (and m)) dests)
    Just (Broadcaster, dests) ->
        (s, pulses dest p dests)

countPulse :: Pulse -> State -> State
countPulse (_, True, _) s = s { highs = highs s + 1 }
countPulse (_, False, _) s = s { lows = lows s + 1 }

-- Process a pulse from the queue, adding any resulting pulses to the
-- back of the queue.
transmitPulse :: Wiring -> (State, PulseQueue) -> Maybe (State, PulseQueue)
transmitPulse wm (s, pq) =
    case Seq.viewl pq of
    EmptyL -> Nothing
    p :< pq' -> case receive wm p s of
        (s', ps) -> Just (s', foldl (|>) pq' ps)

-- Push the button on a state and return the state after all pulses
-- are processed.
pushButton :: Wiring -> State -> State
pushButton wm s = fst (whileJust (transmitPulse wm) (s, initPulseQueue))

summary :: State -> Int
summary s = highs s * lows s

solve1 :: Input -> Int
solve1 wm = summary $ times 1000 (pushButton wm) $ initState wm

testInput1 :: String
testInput1 = "\
    \broadcaster -> a, b, c\n\
    \%a -> b\n\
    \%b -> c\n\
    \%c -> inv\n\
    \&inv -> a\n"

testInput2 :: String
testInput2 = "\
    \broadcaster -> a\n\
    \%a -> inv, con\n\
    \&inv -> b\n\
    \%b -> con\n\
    \&con -> output\n"

tests1 :: [(String, Int)]
tests1 = [(testInput1, 32000000), (testInput2, 11687500)]

-- Part Two

{-
In the puzzle input, the broadcaster feeds four modular counters, each
made of a sequence of flopflops and a NAND gate.  For example, a counter
modulo 53 looks like:

    broadcaster -> b0, ...
    %b0 -> b1, reset
    %b1 -> b2
    %b2 -> b3, reset
    %b3 -> b4
    %b4 -> b5, reset
    %b5 -> reset
    &reset -> out, b0, b2, b4, b5

The sequence of flip-flops hold the bits of the counter, which increments
on each low pulse from the broadcaster.  When the counter reaches 53,
so that the corresponding bits 0, 2, 4 and 5 are set, the reset NAND gate
is triggered, sending a low pulse to out and also resetting those bits,
so that the counter is back to zero.

The puzzle input has four 12-bit counters, connected to NAND gates so
that rx is sent a low pulse when all the counters reset on the same cycle.
-}

-- Get the moduli of the modular counters fed by the broadcaster.
getModuli :: Wiring -> [Int]
getModuli wm = map (bitsToInt . getCounter wm) (snd (wm!rootModule))

-- Follow a chain of flip-flops implementing a modular counter,
-- and extract the bit representation of the modulus.
getCounter :: Wiring -> ModuleName -> [Bool]
getCounter wm n = b : rest
  where
    ns = [(n', fst (wm!n')) | n' <- snd (wm!n)]
    b = or [ty == Conjunction | (_, ty) <- ns]
    rest = case [n' | (n', ty) <- ns, ty == FlipFlop] of
        [] -> []
        (n':_) -> getCounter wm n'

-- convert a list of bits, least significant first, to an integer
bitsToInt :: [Bool] -> Int
bitsToInt bs = sum [2^(k::Int) | (k, b) <- zip [0..] bs, b]

solve2 :: Input -> Int
solve2 wm = foldr1 lcm (getModuli wm)

main :: IO ()
main = do
    s <- readFile "input/20.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    print (solve2 input)
