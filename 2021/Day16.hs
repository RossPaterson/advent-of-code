module Main where

import Utilities
import Control.Monad
import Data.Bits
import Data.Char

-- Input processing

type Input = [Bool]

parse :: String -> Input
parse = concat . map expandBits . map digitToInt . head . lines

expandBits :: Int -> [Bool]
expandBits n = [testBit n i | i <- [3, 2, 1, 0]]

bitsToInt :: [Bool] -> Int
bitsToInt = foldl add 0
  where
    add n b = n*2 + fromIntegral (fromEnum b)

-- Part One

data Packet = Packet Int Expr
    deriving Show

data Expr = Literal Int | Operator Op [Packet]
    deriving Show
type Op = Int

-- strict state monad
newtype State s a = State { runState :: s -> (a, s) }

evalState :: State s a -> s -> a
evalState dec bs = fst (runState dec bs)

instance Functor (State s) where
    fmap f (State r) = State $ \ s -> case r s of
        (x, s') -> (f x, s')

instance Applicative (State s) where
    pure x = State $ \ s -> (x, s)
    (<*>) = ap

instance Monad (State s) where
    State r >>= k = State $ \ s -> case r s of
        (x, s') -> runState (k x) s'

takeOne :: State [a] a
takeOne = State $ \ (b:bs) -> (b, bs)

takeM :: Int -> State [a] [a]
takeM n = State $ splitAt n

atEnd :: State [a] Bool
atEnd = State $ \ s -> (null s, s)

-- repeat the action until the end of the list is reached
untilEnd :: State [a] b -> State [a] [b]
untilEnd m = do
    end <- atEnd
    if end then return [] else (:) <$> m <*> untilEnd m

-- decoding a list of bits
type Decoder = State [Bool]

takeInt :: Int -> Decoder Int
takeInt n = bitsToInt <$> takeM n

packet :: [Bool] -> Packet
packet = evalState decodePacket

decodePacket :: Decoder Packet
decodePacket = Packet <$> takeInt 3 <*> decodeExpr

decodeExpr :: Decoder Expr
decodeExpr = do
    ident <- takeInt 3
    if ident == 4 then (Literal . bitsToInt) <$> getChunkList
    else do
        length_type <- takeOne
        if length_type then do
            n <- takeInt 11
            Operator ident <$> sequence (replicate n decodePacket)
        else do
            len <- takeInt 15
            bs <- takeM len
            return (Operator ident (evalState (untilEnd decodePacket) bs))

getChunkList :: Decoder [Bool]    
getChunkList = do
    bs <- takeM 5
    if head bs then (tail bs ++) <$> getChunkList else return (tail bs)

totalVersion :: Packet -> Int
totalVersion (Packet v e) = v + totalVersionExpr e

totalVersionExpr :: Expr -> Int
totalVersionExpr (Literal _) = 0
totalVersionExpr (Operator _ ps) = sum (map totalVersion ps)

solve1 :: Input -> Int
solve1 = totalVersion . packet

tests1 :: [(String, Int)]
tests1 = [
    ("8A004A801A8002F478", 16),
    ("620080001611562C8802118E34", 12),
    ("C0015000016115A2E0802F182340", 23),
    ("A0016C880162017C3686B18A3D4780", 31)]

-- Part Two

evalPacket :: Packet -> Int
evalPacket (Packet _ e) = evalExpr e

evalExpr :: Expr -> Int
evalExpr (Literal n) = n
evalExpr (Operator ident ps) = case ident of
    0 -> sum vs
    1 -> product vs
    2 -> minimum vs
    3 -> maximum vs
    5 -> fromEnum (binop (>) vs)
    6 -> fromEnum (binop (<) vs)
    7 -> fromEnum (binop (==) vs)
    _ -> error "bad type ID"
  where
    vs = map evalPacket ps

binop :: (a -> a -> b) -> [a] -> b
binop op [x, y] = op x y
binop _ _ = error "nonbinary arg list for binary operator"

solve2 :: Input -> Int
solve2 = evalPacket . packet

tests2 :: [(String, Int)]
tests2 = [
    ("C200B40A82", 3),
    ("04005AC33890", 54),
    ("880086C3E88112", 7),
    ("CE00C43D881120", 9),
    ("D8005AC2A8F0", 1),
    ("F600BC2D8F", 0),
    ("9C005AC2F8F0", 0),
    ("9C0141080250320F1802104A08", 1)]

main :: IO ()
main = do
    s <- readFile "input/16.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
