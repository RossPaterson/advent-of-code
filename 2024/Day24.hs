module Main where

import Graph
import Parser
import Utilities
import Control.Applicative
import Data.Bits
import Data.List
import Data.Map (Map, (!))
import qualified Data.Map as Map

-- Input processing

type Input = (Map Wire Int, Map Wire Gate)

type Wire = String

data Gate = Gate Wire Op Wire
    deriving (Show)

data Op = AND | OR | XOR
    deriving (Bounded, Enum, Eq, Ord, Read, Show)

parse :: String -> Input
parse s = case paragraphs s of
    [p1, p2] ->
        (Map.fromList (map (runParser init_wire) (lines p1)),
         Map.fromList (map (runParser connection) (lines p2)))
    _ -> error "bad input"
  where
    init_wire = (,) <$> wire <* string ": " <*> nat
    connection = flip (,) <$> gate <* string " -> " <*> wire
    gate = Gate <$> wire <* space <*> op <* space <*> wire
    wire = (:) <$> letter <*> many (letter <|> digit)
    op = enumValue

-- Part One

solve1 :: Input -> Int
solve1 (vs, ws) =
    foldr1 (.|.) [shiftL b (read (tail w)) |
        (w, b) <- Map.assocs (eval vs ws), head w == 'z']

eval :: Map Wire Int -> Map Wire Gate -> Map Wire Int
eval vs ws = eval_map
  where
    eval_map = Map.map eval_expr ws
    eval_expr (Gate w1 op w2) = evalOp op (lookup_value w1) (lookup_value w2)
    lookup_value w = Map.findWithDefault (eval_map!w) w vs

evalOp :: Op -> Int -> Int -> Int
evalOp AND = (.&.)
evalOp OR = (.|.)
evalOp XOR = xor

testInput1 :: String
testInput1 = "\
    \x00: 1\n\
    \x01: 1\n\
    \x02: 1\n\
    \y00: 0\n\
    \y01: 1\n\
    \y02: 0\n\
    \\n\
    \x00 AND y00 -> z00\n\
    \x01 XOR y01 -> z01\n\
    \x02 OR y02 -> z02\n\
    \"

testInput2 :: String
testInput2 = "\
    \x00: 1\n\
    \x01: 0\n\
    \x02: 1\n\
    \x03: 1\n\
    \x04: 0\n\
    \y00: 1\n\
    \y01: 1\n\
    \y02: 1\n\
    \y03: 1\n\
    \y04: 1\n\
    \\n\
    \ntg XOR fgs -> mjb\n\
    \y02 OR x01 -> tnw\n\
    \kwq OR kpj -> z05\n\
    \x00 OR x03 -> fst\n\
    \tgd XOR rvg -> z01\n\
    \vdt OR tnw -> bfw\n\
    \bfw AND frj -> z10\n\
    \ffh OR nrd -> bqk\n\
    \y00 AND y03 -> djm\n\
    \y03 OR y00 -> psh\n\
    \bqk OR frj -> z08\n\
    \tnw OR fst -> frj\n\
    \gnj AND tgd -> z11\n\
    \bfw XOR mjb -> z00\n\
    \x03 OR x00 -> vdt\n\
    \gnj AND wpb -> z02\n\
    \x04 AND y00 -> kjc\n\
    \djm OR pbm -> qhw\n\
    \nrd AND vdt -> hwm\n\
    \kjc AND fst -> rvg\n\
    \y04 OR y02 -> fgs\n\
    \y01 AND x02 -> pbm\n\
    \ntg OR kjc -> kwq\n\
    \psh XOR fgs -> tgd\n\
    \qhw XOR tgd -> z09\n\
    \pbm OR djm -> kpj\n\
    \x03 XOR y03 -> ffh\n\
    \x00 XOR y04 -> ntg\n\
    \bfw OR bqk -> z06\n\
    \nrd XOR fgs -> wpb\n\
    \frj XOR qhw -> z04\n\
    \bqk OR frj -> z07\n\
    \y03 OR x01 -> nrd\n\
    \hwm AND bqk -> z03\n\
    \tgd XOR rvg -> z12\n\
    \tnw OR pbm -> gnj\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput1, 4), (testInput2, 2024)]

-- Part Two

-- instructions to Graphviz to display a circuit
visualize :: Map Wire Gate -> String
visualize ws =
    directedGV [(expand w', expand w, []) |
        (w, Gate w1 _ w2) <- Map.assocs ws, w' <- [w1, w2]]
  where
    expand w = case Map.lookup w ws of
        Nothing -> w
        Just (Gate _ op _) -> show op ++ w

{-
The circuit is supposed to be a ripple-carry adder:

    x00  y0   x01  y01
     | \/ |    | \/ |
     | /\ |    | /\ |
    XOR  AND  XOR  AND
     |    | \/ |    |
     |    | /\ |    |
    z00  XOR  AND   |   x02  y02
          |      \  |    | \/ |
          |       \ |    | /\ |
         z01       OR   XOR  AND
                    | \/ |    |
                    | /\ |    |
                   XOR  AND   |   x03  y03
                    |      \  |    | \/ |
                    |       \ |    | /\ |
                   z02       OR   XOR  AND
                              | \/ |    |
                              | /\ |    |
                             XOR  AND   |
                              |      \  |
                              |       \ |
                             z03       OR
                                        ...
                                                x44  y44
                                          \ |    | \/ |
                                           \|    | /\ |
                                            OR  XOR  AND
                                            | \/ |    |
                                            | /\ |    |
                                           XOR  AND   |
                                            |      \  |
                                            |       \ |
                                           z44       OR
                                                      |
                                                      |
                                                     z45

but 8 gates are mislabelled.
-}

solve2 :: Input -> String
solve2 (_, ws) = intercalate "," $ sort $ badLabels ws

badLabels :: Map Wire Gate -> [Wire]
badLabels ws =
    [w | (w, gate) <- Map.assocs ws, targets!w /= expected_targets w gate]
  where
    -- ordered list of actual operators of nodes consuming each wire
    targets :: Map Wire [Op]
    targets =
        compose [Map.adjust (insert op) w |
            Gate w1 op w2 <- Map.elems ws, w <- [w1, w2]] $
        Map.map (const []) ws

    -- ordered list of expected operators of nodes consuming this wire
    expected_targets :: Wire -> Gate -> [Op]
    expected_targets _ (Gate w1 AND w2)
      | sort [w1, w2] == ["x00", "y00"] = [AND, XOR]
      | otherwise = [OR]
    expected_targets w (Gate _ OR _)
      | w == final_output = []
      | otherwise = [AND, XOR]
    expected_targets _ (Gate w1 XOR w2)
      | sort [w1, w2] == ["x00", "y00"] = []
      | sort [head w1, head w2] == "xy" = [AND, XOR]
      | otherwise = []

    (final_output, _) = Map.findMax ws

main :: IO ()
main = do
    s <- readFile "input/24.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStrLn (solve2 input)
