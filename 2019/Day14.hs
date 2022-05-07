module Main where

import Utilities
import Graph
import Parser
import Control.Applicative
import Data.Char
import Data.Map (Map, (!))
import qualified Data.Map as Map
import qualified Data.Set as Set

-- Input processing

type Input = [Reaction]

data Reaction = Reaction [(Int, Chemical)] (Int, Chemical)
    deriving Show
type Chemical = String

parse :: String -> Input
parse = map (runParser reaction) . lines
  where
    reaction = Reaction <$> parts <* string " => " <*> part
    parts = sepBy1 part (string ", ")
    part = (,) <$> int <* char ' ' <*> chemical
    chemical = some (satisfy isUpper)

-- Part One

type Production = Map Chemical (Int, [(Int, Chemical)])

production :: [Reaction] -> Production
production rs = Map.fromList [(res, (n, qs)) | Reaction qs (n, res) <- rs]

-- everything but the last (ORE) in top-down order
productionOrder :: Production -> [Chemical]
productionOrder = init . tsort . Map.map (Set.fromList . map snd . snd)

graphToRelation :: Map a [a] -> [(a, a)]
graphToRelation g = [(x, y) | (x, ys) <- Map.assocs g, y <- ys]

-- add immediate components needed to make the required amount of ch
manufacture :: Production -> Map Chemical Int -> Chemical -> Map Chemical Int
manufacture p required ch = Map.unionWith (+) needed required
  where
    quant = Map.findWithDefault 0 ch required
    (n, comps) = p!ch
    batches = (quant + n - 1) `div` n
    needed = Map.fromList [(c, nc*batches) | (nc, c) <- comps]

-- quantities of all chemicals produced in making n units of c
quants :: Int -> Chemical -> Production -> Map Chemical Int
quants n c p = foldl (manufacture p) (Map.singleton c n) (productionOrder p)

-- amount of ore required to make n units of fuel
oreRequired :: Int -> Production -> Int
oreRequired n p = quants n "FUEL" p ! "ORE"

solve1 :: Input -> Int
solve1 = oreRequired 1 . production

testInput1 :: String
testInput1 = "\
    \10 ORE => 10 A\n\
    \1 ORE => 1 B\n\
    \7 A, 1 B => 1 C\n\
    \7 A, 1 C => 1 D\n\
    \7 A, 1 D => 1 E\n\
    \7 A, 1 E => 1 FUEL\n"

testInput2 :: String
testInput2 = "\
    \9 ORE => 2 A\n\
    \8 ORE => 3 B\n\
    \7 ORE => 5 C\n\
    \3 A, 4 B => 1 AB\n\
    \5 B, 7 C => 1 BC\n\
    \4 C, 1 A => 1 CA\n\
    \2 AB, 3 BC, 4 CA => 1 FUEL\n"

testInput3 :: String
testInput3 = "\
    \157 ORE => 5 NZVS\n\
    \165 ORE => 6 DCFZ\n\
    \44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL\n\
    \12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ\n\
    \179 ORE => 7 PSHF\n\
    \177 ORE => 5 HKGWZ\n\
    \7 DCFZ, 7 PSHF => 2 XJWVT\n\
    \165 ORE => 2 GPVTF\n\
    \3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT\n"

testInput4 :: String
testInput4 = "\
    \2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG\n\
    \17 NVRVD, 3 JNWZP => 8 VPVL\n\
    \53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL\n\
    \22 VJHF, 37 MNCFX => 5 FWMGM\n\
    \139 ORE => 4 NVRVD\n\
    \144 ORE => 7 JNWZP\n\
    \5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC\n\
    \5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV\n\
    \145 ORE => 6 MNCFX\n\
    \1 NVRVD => 8 CXFTF\n\
    \1 VJHF, 6 MNCFX => 4 RFSQX\n\
    \176 ORE => 6 VJHF\n"

testInput5 :: String
testInput5 = "\
    \171 ORE => 8 CNZTR\n\
    \7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL\n\
    \114 ORE => 4 BHXH\n\
    \14 VRPVC => 6 BMBT\n\
    \6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL\n\
    \6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT\n\
    \15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW\n\
    \13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW\n\
    \5 BMBT => 4 WPTQ\n\
    \189 ORE => 9 KTJDG\n\
    \1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP\n\
    \12 VRPVC, 27 CNZTR => 2 XDBXC\n\
    \15 KTJDG, 12 BHXH => 5 XCVML\n\
    \3 BHXH, 2 VRPVC => 7 MZWV\n\
    \121 ORE => 7 VRPVC\n\
    \7 XCVML => 6 RJRHP\n\
    \5 BHXH, 4 VRPVC => 5 LTCX\n"

tests1 :: [(String, Int)]
tests1 = [
    (testInput1, 31), (testInput2, 165), (testInput3, 13312),
    (testInput4, 180697), (testInput5, 2210736)]

-- Part Two

-- amount of fuel that can be produced from a given amount of ore
canProduce :: Int -> Production -> Int
canProduce ore p = fromInteger (bsearch too_many) - 1
  where
    too_many n = n > 0 && oreRequired (fromInteger n) p > ore

solve2 :: Input -> Int
solve2 = canProduce (10^(12::Int)) . production

tests2 :: [(String, Int)]
tests2 = [(testInput3, 82892753), (testInput4, 5586022), (testInput5, 460664)]

main :: IO ()
main = do
    s <- readFile "input/14.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
