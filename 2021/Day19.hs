module Main where

import Utilities
import Geometry
import Parser
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

data Scanner = Scanner Int [Point3]
    deriving Show

type Input = [Scanner]

parse :: String -> Input
parse = map (scanner . lines) . paragraphs
  where
    scanner (header:ps) =
        Scanner (runParser scanner_no header) (map (runParser point) ps)
    scanner [] = error "bad scanner"
    scanner_no = string "--- scanner " *> int <* string " ---"
    point = Point3 <$> int <* char ',' <*> int <* char ',' <*> int

-- Part One

-- a linear transform in 3-dimensional space
data LinearTransform = LinearTransform Point3 Point3 Point3
    deriving Show

apply :: LinearTransform -> Point3 -> Point3
apply (LinearTransform rx ry rz) p = Point3 (dot rx p) (dot ry p) (dot rz p)

-- the 24 rotations that map axes to axes
unitRotations :: [LinearTransform]
unitRotations =
    [LinearTransform rx ry (cross rx ry) |
        rx <- unitVectors, ry <- unitVectors, dot rx ry == 0]

-- offsets to ps2 that produce an overlap of at least 12 points with ps1
overlap_offsets :: [Point3] -> [Point3] -> [Point3]
overlap_offsets ps1 ps2 =
    [d | (d, n) <- frequency [p1 .-. p2 | p1 <- ps1, p2 <- ps2], n >= 12]

-- the point is within range of a sensor at the origin
inRange :: Point3 -> Bool
inRange (Point3 x y z) = abs x <= range && abs y <= range && abs z <= range
  where
    range = 1000

-- two located scanners see the same beacons in the overlap of their ranges
consistent :: (Point3, Scanner) -> (Point3, Scanner) -> Bool
consistent (c1, Scanner _ ps1) (c2, Scanner _ ps2) =
    Set.fromList [p | p <- ps1, inRange (p .-. c2)] ==
        Set.fromList [p | p <- ps2, inRange (p .-. c1)]

data PartialAlignment = PartialAlignment
    [(Point3, Scanner)] -- located scanners with shifted and rotated beacons
    [Scanner] -- unplaced scanners with original beacon positions
    deriving Show

-- start with first scanner at the origin and unrotated
initAlignment :: [Scanner] -> PartialAlignment
initAlignment (s0:ss) = PartialAlignment [(zero, s0)] ss
initAlignment [] = error "no states"

-- locate a scanner that operlaps with some of those already done
locate_one :: PartialAlignment -> [PartialAlignment]
locate_one (PartialAlignment done todo) =
    [PartialAlignment (new:done) (front++back) |
        -- each unlocated scanner
        (front, Scanner n ps:back) <- splits todo,
        -- each rotation of the scanner
        rot <- unitRotations,
        let rot_ps = map (apply rot) ps,
        -- each shift of the rotated scanner overlapping with a located one
        (_, Scanner _ sps) <- done,
        d <- overlap_offsets sps rot_ps,
        let new = (d, Scanner n (map (d .+.) rot_ps)),
        -- check that the shifted rotated scanner fits with those we have
        and [consistent old new | old <- done]]

align :: PartialAlignment -> [[(Point3, Scanner)]]
align (PartialAlignment done []) = [done]
align s = concatMap align (locate_one s)

-- all possible alignments of the scanners
alignments :: [Scanner] -> [[(Point3, Scanner)]]
alignments = align . initAlignment

-- all beacons seen by any located scanner
points :: [(Point3, Scanner)] -> Set Point3
points ss = Set.unions [Set.fromList ps | (_, Scanner _ ps) <- ss]

solve1 :: Input -> Int
solve1 = Set.size . points . head . alignments

testInput :: String
testInput = "\
    \--- scanner 0 ---\n\
    \404,-588,-901\n\
    \528,-643,409\n\
    \-838,591,734\n\
    \390,-675,-793\n\
    \-537,-823,-458\n\
    \-485,-357,347\n\
    \-345,-311,381\n\
    \-661,-816,-575\n\
    \-876,649,763\n\
    \-618,-824,-621\n\
    \553,345,-567\n\
    \474,580,667\n\
    \-447,-329,318\n\
    \-584,868,-557\n\
    \544,-627,-890\n\
    \564,392,-477\n\
    \455,729,728\n\
    \-892,524,684\n\
    \-689,845,-530\n\
    \423,-701,434\n\
    \7,-33,-71\n\
    \630,319,-379\n\
    \443,580,662\n\
    \-789,900,-551\n\
    \459,-707,401\n\
    \\n\
    \--- scanner 1 ---\n\
    \686,422,578\n\
    \605,423,415\n\
    \515,917,-361\n\
    \-336,658,858\n\
    \95,138,22\n\
    \-476,619,847\n\
    \-340,-569,-846\n\
    \567,-361,727\n\
    \-460,603,-452\n\
    \669,-402,600\n\
    \729,430,532\n\
    \-500,-761,534\n\
    \-322,571,750\n\
    \-466,-666,-811\n\
    \-429,-592,574\n\
    \-355,545,-477\n\
    \703,-491,-529\n\
    \-328,-685,520\n\
    \413,935,-424\n\
    \-391,539,-444\n\
    \586,-435,557\n\
    \-364,-763,-893\n\
    \807,-499,-711\n\
    \755,-354,-619\n\
    \553,889,-390\n\
    \\n\
    \--- scanner 2 ---\n\
    \649,640,665\n\
    \682,-795,504\n\
    \-784,533,-524\n\
    \-644,584,-595\n\
    \-588,-843,648\n\
    \-30,6,44\n\
    \-674,560,763\n\
    \500,723,-460\n\
    \609,671,-379\n\
    \-555,-800,653\n\
    \-675,-892,-343\n\
    \697,-426,-610\n\
    \578,704,681\n\
    \493,664,-388\n\
    \-671,-858,530\n\
    \-667,343,800\n\
    \571,-461,-707\n\
    \-138,-166,112\n\
    \-889,563,-600\n\
    \646,-828,498\n\
    \640,759,510\n\
    \-630,509,768\n\
    \-681,-892,-333\n\
    \673,-379,-804\n\
    \-742,-814,-386\n\
    \577,-820,562\n\
    \\n\
    \--- scanner 3 ---\n\
    \-589,542,597\n\
    \605,-692,669\n\
    \-500,565,-823\n\
    \-660,373,557\n\
    \-458,-679,-417\n\
    \-488,449,543\n\
    \-626,468,-788\n\
    \338,-750,-386\n\
    \528,-832,-391\n\
    \562,-778,733\n\
    \-938,-730,414\n\
    \543,643,-506\n\
    \-524,371,-870\n\
    \407,773,750\n\
    \-104,29,83\n\
    \378,-903,-323\n\
    \-778,-728,485\n\
    \426,699,580\n\
    \-438,-605,-362\n\
    \-469,-447,-387\n\
    \509,732,623\n\
    \647,635,-688\n\
    \-868,-804,481\n\
    \614,-800,639\n\
    \595,780,-596\n\
    \\n\
    \--- scanner 4 ---\n\
    \727,592,562\n\
    \-293,-554,779\n\
    \441,611,-461\n\
    \-714,465,-776\n\
    \-743,427,-804\n\
    \-660,-479,-426\n\
    \832,-632,460\n\
    \927,-485,-438\n\
    \408,393,-506\n\
    \466,436,-512\n\
    \110,16,151\n\
    \-258,-428,682\n\
    \-393,719,612\n\
    \-211,-452,876\n\
    \808,-476,-593\n\
    \-575,615,604\n\
    \-485,667,467\n\
    \-680,325,-822\n\
    \-627,-443,-432\n\
    \872,-547,-609\n\
    \833,512,582\n\
    \807,604,487\n\
    \839,-516,451\n\
    \891,-625,532\n\
    \-652,-548,-490\n\
    \30,-46,-14\n\
    \"

tests1 :: [(String, Int)]
tests1 = [(testInput, 79)]

-- Part Two

max_dist :: [Point3] -> Int
max_dist ps = maximum [distance p1 p2 | p1 <- ps, p2 <- ps]

solve2 :: Input -> Int
solve2 = max_dist . map fst . head . alignments

tests2 :: [(String, Int)]
tests2 = [(testInput, 3621)]

main :: IO ()
main = do
    s <- readFile "input/19.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
