-- Sudoku solver
-- Based on Bird, "Thinking Functionally with Haskell"

import Data.List (sort,nub,(\\),transpose,genericLength)
import Data.String (lines,unlines)
import Test.QuickCheck

-- Representing Sudoku puzzles

type Row a    = [a]
type Matrix a = [Row a]
type Digit    = Char

-- Examples from websudoku.com

easy :: Matrix Digit
easy = ["    345  ",
        "  89   3 ",
        "3    2789",
        "2 4  6815",
        "    4    ",
        "8765  4 2",
        "7523    6",
        " 1   79  ",
        "  942    "]

medium :: Matrix Digit
medium = ["   4 6 9 ",
          "     3  5",
          "45     86",
          "6 2 74  1",
          "    9    ",
          "9  56 7 8",
          "71     64",
          "3  6     ",
          " 6 9 2   "]

hard :: Matrix Digit
hard = ["9 3  42  ",
        "4 65     ",
        "  28     ",
        "     5  4",
        " 67 4 92 ",
        "1  9     ",
        "     87  ",
        "     94 3",
        "  83  6 1"]

evil :: Matrix Digit
evil = ["  9      ",
        "384   5  ",
        "    4 3  ",
        "   1  27 ",
        "2  3 4  5",
        " 48  6   ",
        "  6 1    ",
        "  7   629",
        "     5   "]

-- Another example, from Bird's book

book :: Matrix Digit
book = ["  4  57  ",
        "     94  ",
        "36      8",
        "72  6    ",
        "   4 2   ",
        "    8  93",
        "4      56",
        "  53     ",
        "  61  9  "]

-- Printing Sudoku puzzles

-- -- I made this nicer with box-drawing characters and spacing

group :: [a] -> [[a]]
group = groupBy 3

groupBy :: Int -> [a] -> [[a]]
groupBy n [] = []
groupBy n xs = take n xs : groupBy n (drop n xs)

intersperse :: a -> [a] -> [a]
intersperse sep []     = [sep]
intersperse sep (y:ys) = sep : y : intersperse sep ys

showRow :: String -> String
showRow = concat . intersperse "│" . map (intersperse ' ') . group

showGrid :: Matrix Digit -> [String]
showGrid = (++ [bottom]) . take 12 . (top :) . drop 1 . showCol . map showRow
  where
    showCol = concat . intersperse [bar] . group
    top     = "┌───────┬───────┬───────┐"
    bar     = "├───────┼───────┼───────┤"
    bottom  = "└───────┴───────┴───────┘"

put :: Matrix Digit -> IO ()
put = putStrLn . unlines . showGrid

choice :: Digit -> [Digit]
choice ' ' = "123456789"
choice x   | x `elem` choice ' ' = [x]
           | otherwise = error "Digit not a digit"

choices :: Matrix Digit -> Matrix [Digit]
choices = map (map choice)

splits :: [a] -> [(a, [a])]
splits xs  =
  [ (xs!!k, take k xs ++ drop (k+1) xs) | k <- [0..n-1] ]
  where
  n = length xs

pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = map (remove [x | (x:xs, _) <- splits row, null xs]) row
  where
    remove :: [Digit] -> [Digit] -> [Digit]
    remove xs [x] = [x]
    remove xs ys  = ys \\ xs

-- this code builds on pruneRow to also prune columns and boxes

pruneBy :: (Matrix [Digit] -> Matrix [Digit]) -> Matrix [Digit] -> Matrix [Digit]
pruneBy f = f . map pruneRow . f

rows, cols, boxs :: Matrix a -> Matrix a
rows = id
cols = transpose
boxs = map concat . concat . map cols . group . map group

prune :: Matrix [Digit] -> Matrix [Digit]
prune = pruneBy boxs . pruneBy cols . pruneBy rows

close :: Eq a => (a -> a) -> a -> a
close g x | x == g x  = x
          | otherwise = close g (g x)

extract :: Matrix [Digit] -> Matrix Digit
extract = map concat

matrix :: (a -> b -> c) -> Matrix a -> Matrix b -> Matrix c
matrix f = zipWith (zipWith f)

matrixOr :: Matrix [Digit] -> Matrix [Digit] -> Matrix [Digit]
matrixOr m0 m1 = matrixMap boolToDig $ matrix (||) (matrixMap digToBool m0) (matrixMap digToBool m1)

matrixMap f = map (map f)

boolToDig :: Bool -> [Digit]
boolToDig True  = ['1']
boolToDig False = ['0']

digToBool :: [Digit] -> Bool
digToBool ['1'] = True
digToBool ['0'] = False

failed :: Matrix [Digit] -> Bool
failed = foldr (||) False . map (foldr (||) False) . allDigits
    where
        allDigits :: Matrix [Digit] -> Matrix Bool
        allDigits mat = matrixMap digToBool $ (allDigitsBy boxs mat `matrixOr` allDigitsBy cols mat) `matrixOr` allDigitsBy rows mat

        allDigitsBy :: (Matrix [Digit] -> Matrix [Digit]) -> Matrix [Digit] -> Matrix [Digit]
        allDigitsBy f = f . map allDigitsRow . f

        allDigitsRow :: [[Digit]] -> [[Digit]]
        allDigitsRow xs = [boolToDig ((choice ' ' !! i) `notElem` concat xs) | i <- [0..8]]

solved :: Matrix [Digit] -> Bool
solved m | failed m  = False
         | otherwise = foldr (&&) True . map (foldr (&&) True . map (null . tail)) $ m

shortest :: Matrix [Digit] -> Int
shortest = foldr1 min . filter (/= 1) . map length . concat

expand :: Matrix [Digit] -> [Matrix [Digit]]
expand mat | solved mat = [mat]
           | otherwise  = [preMat ++ [preRow ++ [[d]] ++ postRow] ++ postMat | d <- ds]
    where
        (preMat, row:postMat) = break (any p) mat
        (preRow, ds:postRow)  = break p row
        p :: [Digit] -> Bool
        p xs = length xs == shortest mat

findSolutions :: [Matrix [Digit]] -> [Matrix [Digit]]
findSolutions []     = []
findSolutions (x:xs) | failed x  = findSolutions xs
                     | solved x  = x : findSolutions xs
                     | otherwise = findSolutions (map (close prune) (expand x) ++ xs)

solve :: Matrix Digit -> [Matrix Digit]
solve mat | failed mat' = []
          | solved mat' = [extract mat']
          | otherwise   = nub (map extract (findSolutions [mat']))
        where 
           mat' = (close prune (choices mat))

puzzle :: Matrix Digit -> IO ()
puzzle g = put g >> puts (solve g) >> putStrLn "***"
     where puts = sequence_ . map put
       
main :: IO ()
main = puzzle easy >>
       puzzle medium >>
       puzzle hard >>
       puzzle evil >>
       puzzle book

