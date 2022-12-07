module Main where

import Utilities
import Parser
import Control.Applicative
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

-- Input processing

type Input = [Command]
data Command = Chdir Filename | List [Entry]
    deriving (Show)
data Entry = Dir Filename | File Int Filename
    deriving (Show)
type Filename = String

parse :: String -> Input
parse s = unfoldr getCommand (lines s)
  where
    getCommand [] = Nothing
    getCommand ("$ ls":rest) =
        Just (List (map (runParser entry) entries), commands)
      where
        (entries, commands) = span (not . ("$ " `isPrefixOf`)) rest
    getCommand (cmd:rest) = Just (runParser chdir cmd, rest)
    chdir = Chdir <$ string "$ cd " <*> filename
    entry =
        Dir <$ string "dir " <*> filename <|>
        File <$> nat <* space <*> filename
    filename = some (satisfy (/= ' '))

-- Part One

-- Tree representing a hierarchical filesystem

type Tree = Map Filename TreeEntry
data TreeEntry = TreeDir Tree | TreeFile Int
    deriving (Show)

showTree :: Tree -> String
showTree t = unlines ("- / (dir)":showTreeContents 1 t)

showTreeContents :: Int -> Tree -> [String]
showTreeContents depth t =
    concat [showEntry name contents | (name, contents) <- Map.assocs t]
  where
    showEntry name (TreeDir subtree) =
        (indent ++ name ++ " (dir)") : showTreeContents (depth+1) subtree
    showEntry name (TreeFile size) =
        [indent ++ name ++ " (file, size=" ++ show size ++ ")"]
    indent = replicate (2*depth) ' '

-- Building the tree from a list of commands

data State = State { wd :: WorkingDir, fs :: Tree }
    deriving (Show)
type WorkingDir = [Filename] -- from bottom up

buildTree :: [Command] -> Tree
buildTree = fs . foldl doCmd initState

initState :: State
initState = State [] Map.empty

doCmd :: State -> Command -> State
doCmd s (Chdir d) = s { wd = doChdir d (wd s) }
doCmd s (List entries) =
    s { fs = unionTree (fs s) (addPath (mkDirectory entries) (wd s)) }

doChdir :: Filename -> WorkingDir -> WorkingDir
doChdir "/" _ = []
doChdir ".." [] = [] -- not in the input, but this is the Unix semantics
doChdir ".." (_:ds) = ds
doChdir d ds = d:ds

-- place the tree at the position of the working directory
addPath :: Tree -> WorkingDir -> Tree
addPath = foldl (\ t d -> Map.singleton d (TreeDir t))

mkDirectory :: [Entry] -> Tree
mkDirectory entries = Map.fromList (map mkEntry entries)

mkEntry :: Entry -> (Filename, TreeEntry)
mkEntry (Dir name) = (name, TreeDir Map.empty)
mkEntry (File size name) = (name, TreeFile size)

unionTree :: Tree -> Tree -> Tree
unionTree = Map.unionWith unionEntry

unionEntry :: TreeEntry -> TreeEntry -> TreeEntry
unionEntry (TreeDir t1) (TreeDir t2) = TreeDir (unionTree t1 t2)
unionEntry _ t = t

-- preorder listing of the total sizes of directories in the tree
sizes :: Tree -> [Int]
sizes t = total:concat dir_sizes
  where
    entries = Map.elems t
    dir_sizes = [sizes subtree | TreeDir subtree <- entries]
    file_sizes = sum [size | TreeFile size <- entries]
    total = sum (map head dir_sizes) + file_sizes

solve1 :: Input -> Int
solve1 cmds = sum $ filter (<= 100000) $ sizes $ buildTree cmds

testInput :: String
testInput = "\
    \$ cd /\n\
    \$ ls\n\
    \dir a\n\
    \14848514 b.txt\n\
    \8504156 c.dat\n\
    \dir d\n\
    \$ cd a\n\
    \$ ls\n\
    \dir e\n\
    \29116 f\n\
    \2557 g\n\
    \62596 h.lst\n\
    \$ cd e\n\
    \$ ls\n\
    \584 i\n\
    \$ cd ..\n\
    \$ cd ..\n\
    \$ cd d\n\
    \$ ls\n\
    \4060174 j\n\
    \8033020 d.log\n\
    \5626152 d.ext\n\
    \7214296 k\n"

tests1 :: [(String, Int)]
tests1 = [(testInput, 95437)]

-- Part Two

solve2 :: Input -> Int
solve2 cmds = minimum (filter (>= needed) ns)
  where
    ns = sizes (buildTree cmds)
    total = head ns
    needed = total - 40000000

tests2 :: [(String, Int)]
tests2 = [(testInput, 24933642)]

main :: IO ()
main = do
    s <- readFile "input/07.txt"
    let input = parse s
    putStr (unlines (failures "solve1" (solve1 . parse) tests1))
    print (solve1 input)
    putStr (unlines (failures "solve2" (solve2 . parse) tests2))
    print (solve2 input)
