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

     x0  y0    x1  y1
     | \/ |    | \/ |
     | /\ |    | /\ |
    XOR  AND  XOR  AND
     |    | \/ |    |
     |    | /\ |    |
     z0  XOR  AND   |    x2  y2
          |      \  |    | \/ |
          |       \ |    | /\ |
          z1       OR   XOR  AND
                    | \/ |    |
                    | /\ |    |
                   XOR  AND   |    x3  y3
                    |      \  |    | \/ |
                    |       \ |    | /\ |
                    z2       OR   XOR  AND
                              | \/ |    |
                              | /\ |    |
                             XOR  AND   |
                              |      \  |
                              |       \ |
                              z3       OR
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
    [w | (w, Gate _ op _) <- Map.assocs ws, bad_label w op]
  where
    (final_output, final_gate) = Map.findMax ws

    bad_label w@('z':_) op
      | w == final_output = op /= OR
      | otherwise = op /= XOR
    bad_label w XOR
      | misplaced_xor w = True
    bad_label w op =
        outdegree ws w /= expected_outdegree w op

    expected_outdegree ('z':_) _ = 0
    expected_outdegree w AND
      | uses "x00" gate && uses "y00" gate = 2
      | otherwise = 1
      where
        gate = ws!w
    expected_outdegree w OR
      | uses w final_gate = 1
      | otherwise = 2
    expected_outdegree _ XOR = 2

    -- hack: an XOR that should be an output, but isn't
    misplaced_xor w = case ws!w of
        Gate w1 XOR w2 ->
            sort [liftA getOp (Map.lookup w1 ws),
                liftA getOp (Map.lookup w2 ws)] == [Just OR, Just XOR]
        _ -> False

outdegree :: Map Wire Gate -> Wire -> Int
outdegree ws w = length (filter (uses w) (Map.elems ws))

uses :: Wire -> Gate -> Bool
uses w (Gate w1 _ w2) = w == w1 || w == w2

getOp :: Gate -> Op
getOp (Gate _ op _) = op

main :: IO ()
main = do
    s <- readFile "input/24.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStrLn (solve2 input)
