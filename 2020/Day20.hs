module Main where

import Geometry
import Utilities
import Parser
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

-- Input processing

type Input = Map Int Tile

-- square grid of values
type Grid a = [[a]]
type Tile = Grid Bool

parse :: String -> Input
parse = Map.fromList . map parseCamera . paragraphs

parseCamera :: String -> (Int, Tile)
parseCamera s = (runParser tileno header, parseTile rest)
  where
    header:rest = lines s
    tileno = string "Tile " *> nat <* char ':'

parseTile :: [String] -> Tile
parseTile = map (map (== '#'))

showTile :: Tile -> String
showTile = unlines . map (map showBit)
  where
    showBit False = '.'
    showBit True = '#'

-- Part One

-- all transformations (flips and rotations) of a tile
transforms :: Tile -> [Tile]
transforms tile =
    [times rot rotateGrid $ times fl transpose tile |
        fl <- [0, 1], rot <- [0..3]]

-- rotate clockwise
rotateGrid :: Grid a -> Grid a
rotateGrid = transpose . reverse

-- tiles having a particular value for the property
tileMap :: Ord a => (Tile -> a) -> Map Int [Tile] -> Map a [(Int, Tile)]
tileMap property m =
    Map.fromListWith (++)
        [(property tile, [(n, tile)]) |
            (n, tiles) <- Map.assocs m, tile <- tiles]

-- Find an arrangement of tiles (possibly transformed) so that adjacent
-- edges match.  We build up a grid of transformed tiles, starting at
-- the top left and proceeding left-to-right on each rom from the top
-- to the bottom.
arrangeTiles :: Map Int Tile -> Grid (Int, Tile)
arrangeTiles m = takes side $ reverse $ head $
    [res |
        top_row <- top_rows,
        res <- add_rest (Set.fromList (map fst top_row)) top_row]
  where
    -- possible top rows (each reversed)
    top_rows :: [[(Int, Tile)]]
    top_rows = [row | start <- all_tiles, row <- mk_row 1 [start]]

    -- possible extensions to top row, at each stage looking up tiles
    -- matching the right edge of the preceding tile in the row
    mk_row :: Int -> [(Int, Tile)] -> [[(Int, Tile)]]
    mk_row n done
      | n == side = [done]
      | otherwise = [res |
            ntile <- Map.findWithDefault []
                (right_border (snd (head done))) left_border_map,
            all ((/= fst ntile) . fst) done,
            res <- mk_row (n+1) (ntile:done)]

    -- possible extensions to other rows, at each stage looking up tiles
    -- matching the bottom edge of the tile above
    add_rest :: Set Int -> [(Int, Tile)] -> [[(Int, Tile)]]
    add_rest ts done
      | Set.size ts == Map.size m = [done]
      | otherwise = [res |
            (n, tile) <- Map.findWithDefault []
                (bottom_border (snd (done!!(side-1)))) top_border_map,
            not (Set.member n ts),
            col == 0 || left_border tile == right_border (snd (head done)),
            res <- add_rest (Set.insert n ts) ((n, tile):done)]
      where
        col = Set.size ts `mod` side

    all_tiles :: [(Int, Tile)]
    all_tiles =
        [(n, tile) | (n, tiles) <- Map.assocs all_transforms, tile <- tiles]

    all_transforms = Map.map transforms m
    top_border_map = tileMap top_border all_transforms
    left_border_map = tileMap left_border all_transforms
    side = intSqrt (Map.size m)

left_border :: Grid a -> [a]
left_border = map head

right_border :: Grid a -> [a]
right_border = map last

top_border :: Grid a -> [a]
top_border = head

bottom_border :: Grid a -> [a]
bottom_border = last

intSqrt :: Int -> Int
intSqrt n = head [i | i <- [0..], i*i >= n]

mul_corners :: Grid Int -> Int
mul_corners its = row top * row bottom
  where
    row is = head is * last is
    top = head its
    bottom = last its

solve1 :: Input -> Int
solve1 = mul_corners . map (map fst) . arrangeTiles

testInput :: String
testInput = "\
    \Tile 2311:\n\
    \..##.#..#.\n\
    \##..#.....\n\
    \#...##..#.\n\
    \####.#...#\n\
    \##.##.###.\n\
    \##...#.###\n\
    \.#.#.#..##\n\
    \..#....#..\n\
    \###...#.#.\n\
    \..###..###\n\
    \\n\
    \Tile 1951:\n\
    \#.##...##.\n\
    \#.####...#\n\
    \.....#..##\n\
    \#...######\n\
    \.##.#....#\n\
    \.###.#####\n\
    \###.##.##.\n\
    \.###....#.\n\
    \..#.#..#.#\n\
    \#...##.#..\n\
    \\n\
    \Tile 1171:\n\
    \####...##.\n\
    \#..##.#..#\n\
    \##.#..#.#.\n\
    \.###.####.\n\
    \..###.####\n\
    \.##....##.\n\
    \.#...####.\n\
    \#.##.####.\n\
    \####..#...\n\
    \.....##...\n\
    \\n\
    \Tile 1427:\n\
    \###.##.#..\n\
    \.#..#.##..\n\
    \.#.##.#..#\n\
    \#.#.#.##.#\n\
    \....#...##\n\
    \...##..##.\n\
    \...#.#####\n\
    \.#.####.#.\n\
    \..#..###.#\n\
    \..##.#..#.\n\
    \\n\
    \Tile 1489:\n\
    \##.#.#....\n\
    \..##...#..\n\
    \.##..##...\n\
    \..#...#...\n\
    \#####...#.\n\
    \#..#.#.#.#\n\
    \...#.#.#..\n\
    \##.#...##.\n\
    \..##.##.##\n\
    \###.##.#..\n\
    \\n\
    \Tile 2473:\n\
    \#....####.\n\
    \#..#.##...\n\
    \#.##..#...\n\
    \######.#.#\n\
    \.#...#.#.#\n\
    \.#########\n\
    \.###.#..#.\n\
    \########.#\n\
    \##...##.#.\n\
    \..###.#.#.\n\
    \\n\
    \Tile 2971:\n\
    \..#.#....#\n\
    \#...###...\n\
    \#.#.###...\n\
    \##.##..#..\n\
    \.#####..##\n\
    \.#..####.#\n\
    \#..#.#..#.\n\
    \..####.###\n\
    \..#.#.###.\n\
    \...#.#.#.#\n\
    \\n\
    \Tile 2729:\n\
    \...#.#.#.#\n\
    \####.#....\n\
    \..#.#.....\n\
    \....#..#.#\n\
    \.##..##.#.\n\
    \.#.####...\n\
    \####.#.#..\n\
    \##.####...\n\
    \##..#.##..\n\
    \#.##...##.\n\
    \\n\
    \Tile 3079:\n\
    \#.#.#####.\n\
    \.#..######\n\
    \..#.......\n\
    \######....\n\
    \####.#..#.\n\
    \.#...#.##.\n\
    \#.#####.##\n\
    \..#.###...\n\
    \..#.......\n\
    \..#.###...\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 20899048083289)]

-- Part Two

-- Create an image by:
-- (1) finding the correct arrangement of tiles
-- (2) removing the border from each tile
-- (3) glueing the tiles together
getImage :: Map Int Tile -> Tile
getImage = glueTiles . map (map (removeBorder . snd)) . arrangeTiles

-- remove the border from a grid
removeBorder :: Grid a -> Grid a
removeBorder = map (init . tail) . init . tail

-- glue together a grid of grids
glueTiles :: Grid (Grid a) -> Grid a
glueTiles = concat . map (map concat . transpose)

-- convert a tile to a set of positions of the ones
ones :: Tile -> Set Position
ones bss =
    Set.fromList [Position col row |
        (row, bs) <- zip [0..] bss, (col, b) <- zip [0..] bs, b]

-- the search pattern (sea monster)

seaMonsterText :: String
seaMonsterText = "\
    \                  # \n\
    \#    ##    ##    ###\n\
    \ #  #  #  #  #  #   \n"

seaMonster :: Set Position
seaMonster = ones $ parseTile $ lines seaMonsterText

-- number of occurrences of the pattern in the image,
-- assuming that occurrences do not overlap
occurrences :: Set Position -> Set Position -> Int
occurrences pattern image =
    length [offset | offset <- offsets,
        translateImage offset pattern `Set.isSubsetOf` image]
  where
    offsets = [Position x y | x <- [0..xmax], y <- [0..ymax]]
    xmax = max_x image - max_x pattern
    ymax = max_y image - max_y pattern

-- translate a set of positions by an offset
translateImage :: Position -> Set Position -> Set Position
translateImage offset = Set.mapMonotonic (offset .+.)

max_x :: Set Position -> Int
max_x ps = maximum [x | Position x _ <- Set.elems ps]

max_y :: Set Position -> Int
max_y ps = maximum [y | Position _ y <- Set.elems ps]

solve2 :: Input -> Int
solve2 m =
    head [nbits - n*Set.size seaMonster |
        trans_tile <- transforms tile,
        let n = occurrences seaMonster (ones trans_tile),
        n > 0]
  where
    tile = getImage m
    nbits = sum (map (length . filter id) tile)

tests2 :: [(String, Int)]
tests2 = [(testInput, 273)]

main :: IO ()
main = do
    s <- readFile "input/20.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
