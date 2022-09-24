module Sudoku where

-- Sudoku solver
-- Based on Bird, "Thinking Functionally with Haskell"

import Data.List (sort,nub,(\\),transpose,genericLength)
import Data.String (lines,unlines)
import Test.QuickCheck

type Row a    = [a]
type Matrix a = [Row a]
type Digit    = Char

digits :: [Digit]
digits = ['1'..'9']

blank :: Digit -> Bool
blank d = d == ' '

-- 1.
group :: [a] -> [[a]]
group = groupBy 3

groupBy :: Int -> [a] -> [[a]]
groupBy _ [] = []
groupBy n xs = take n xs : groupBy n (drop n xs)

-- 2.
intersperse :: a -> [a] -> [a]
intersperse ch []      = [ch]
intersperse ch (x:xs) = ch : x : intersperse ch xs

-- 3.
showRow :: String -> String
showRow str = concat (intersperse "|" (group str))

-- 4.
showGrid :: Matrix Digit -> [String]
showGrid mtrx = concat (intersperse [replicate 13 '-'] (group mtrx))

-- 5.
put :: Matrix Digit -> IO ()
put mtrx = putStrLn (unlines (showGrid [showRow x | x <- mtrx]))
        

-- 6.
showMat :: Matrix Digit -> String
showMat mtrx = map replace (concat mtrx)
        where replace ' ' = '.'
              replace  x  =  x

readMat :: String -> Matrix Digit
readMat str = groupBy 9 (map replace str)
        where replace '.' = ' '
              replace  x  =  x


-- 7.
choices :: Matrix Digit -> Matrix [Digit]
choices mtrx = [[ replace y | y <- x] | x <- groupBy 9 (showMat mtrx)]
        where replace '.' = "123456789"
              replace  x  =  [x]

-- 8.
cp :: [[a]] -> [[a]]
cp []       = [[]]
cp (xs:xss) = [ x:ys | x <- xs, ys <- cp xss ]

prop_cp :: [[a]] -> Bool
prop_cp xss = length (cp xss) == product (map length xss)
-- slow: use `quickCheckWith (stdArgs {maxSize = 5})`

expand :: Matrix [Digit] -> [Matrix Digit]
expand []     = [[]]
expand (x:xs) = [ y : z | y <- cp x, z <- expand xs]

-- 9.
prop_expand :: Matrix [Digit] -> Bool
prop_expand mtrx = length (expand mtrx) == product (map length (concat mtrx))
        

-- 10.
easySols :: Integer
easySols = product (map (fromIntegral . length) (concat (choices easy)))


-- 11, 12, 13.
rows, cols, boxs :: Matrix a -> Matrix a
rows mtrx = mtrx
cols      = transpose
boxs mtrx = map ungroup (ungroup (map cols (group (map group mtrx))))
  where
    ungroup :: Matrix a -> [a]
    ungroup = concat

-- 14.
distinct :: Eq a => [a] -> Bool
distinct xs = nub xs == xs

-- 15.
valid :: Matrix Digit -> Bool
valid g = and[distinct x | x <- boxs g] && and[distinct x | x <- g] && and[distinct x | x <- cols g] 

-- 16.
simple :: Matrix Digit -> [Matrix Digit]
simple = filter valid . expand . choices
--Yes as it finds all the matrices that satisfy the rules of a sudoku puzzle.

-- 17.
the :: [Digit] -> Digit
the [d] = d

del :: Digit -> [Digit] -> [Digit] 
del n [] = []
del n (x:xs) 
        | x == n   = del n xs
        |otherwise = x : del n xs

singles :: Row [Digit] -> Row [Digit]
singles row = [ x | x <- row, length x == 1]
        
removes :: Row [Digit] -> [Digit] -> [Digit]
removes xs ys = foldl (\ ys x -> del (the x) ys) ys xs

        
pruneRow :: Row [Digit] -> Row [Digit]
pruneRow [] = []
pruneRow (x:xs) 
        | length x > 1 =  removes (singles (x:xs)) x : pruneRow' xs
        | otherwise    = x : pruneRow' xs
                where 
                        pruneRow' [] = []
                        pruneRow' (y:ys) 
                                | length y > 1 =  removes (singles (x:xs)) y : pruneRow' ys
                                | otherwise    = y : pruneRow' ys

-- 18.
pruneBy :: (Matrix [Digit] -> Matrix [Digit]) -> Matrix [Digit] -> Matrix [Digit]
pruneBy f = f . map pruneRow . f

prune :: Matrix [Digit] -> Matrix [Digit]
prune mtrx = foldr pruneBy mtrx [rows, cols, boxs]

-- 19.
many :: Eq a => (a -> a) -> a -> a
many g x 
        | g x == x  = x
        | otherwise = many g (g x)

-- 20.
the' :: [Digit] -> Digit
the' [d] = d
the'  _  = undefined

extract :: Matrix [Digit] -> Matrix Digit
extract = map (map the')

-- 21.
solve :: Matrix Digit -> Matrix Digit
solve mtrx = extract (many prune (choices mtrx))
--Easy, medium and book are able to be solved

-- ** Optional Material

-- 22.
failed :: Matrix [Digit] -> Bool
failed mat = or[ or[ null y | y <- x] | x <- mat]

-- 23.
solved :: Matrix [Digit] -> Bool
solved mtrx = and[ and[ length y == 1 | y <- x]| x <- mtrx]

-- 24.
shortest :: Matrix [Digit] -> Int
shortest mtrx = minimum[ minimum[ length y | y <- x]| x <- mtrx]

-- 25.
expand1 :: Matrix [Digit] -> [Matrix [Digit]]
expand1 mat = [preMat ++ [preRow ++ [[d]] ++ postRow] ++ postMat | d <- ds]
        where
                p x = length x == shortest mat
                (preMat, row:postMat) = break (any p) mat
                (preRow, ds:postRow) = break p row


-- 26.

search :: Matrix Digit -> [Matrix Digit]
search mtrx = search' . many prune . choices mtrx
        where
        search' m  
                | solved m = [extract m]
                | failed m = []
                | otherwise = concat (map (search' . many prune) (expand1 m))

        


-- Example from Bird

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

puts :: [Matrix Digit] -> IO ()
puts = sequence_ . map put

puzzle :: Matrix Digit -> IO ()
puzzle g = put g >> puts (search g) >> putStrLn "***"
       
main :: IO ()
main = puzzle easy >>
       puzzle medium >>
       puzzle hard >>
       puzzle evil
