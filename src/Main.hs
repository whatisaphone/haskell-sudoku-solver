-- Sudoku solver

module Main where

import Data.Char
import qualified Data.Set as Set

-- Each of the 81 cells contains either a definite value, or an unknown
-- Sure, we could use Maybe Int, but doing it this way keeps things explicit

data Value = IntVal Int | UnknownVal
data Grid = Grid [Value]

isKnownVal val = case val of {IntVal _ -> True; _ -> False}
flattenVal val = case val of {IntVal num -> [num]; _ -> []}

-- I/O support code

parseGrid :: String -> [Value]
parseGrid source = fst $ collectValues 81 source
    where
        collectValues :: Int -> String -> ([Value], String)
        collectValues left src = if left == 0 then ([], src) else (num:nums, src'')
             where (num, src') = readValue src
                   (nums, src'') = collectValues (pred left) src'
        readValue (x:xs)
            | isDigit x = (IntVal (read [x]), xs)
            | x == '_'  = (UnknownVal, xs)
            | isSpace x = readValue xs
        readValue _ = error "invalid input"

instance Show Grid where
    show (Grid nums) = snd $ foldr join (0 :: Int, "") nums
        where join val (i, xs) = (succ i, showVal val ++ sep ++ xs)
                where sep | i `mod` 27 == 0 = "\n\n"
                          | i `mod` 9 == 0  = "\n"
                          | i `mod` 3 == 0  = "  "
                          | otherwise       = " "
              showVal (IntVal num) = show num
              showVal UnknownVal   = "_"

-- return the given list, with the `i`th element replaced by `el`
replaceElem :: Int -> a -> [a] -> [a]
replaceElem i el (x:xs)
    | i == 0    = el:xs
    | otherwise = x : replaceElem (pred i) el xs
replaceElem _ _ _ = error "end of list reached"

-- cells are numbered 0 to 80, with the top row [0..8], next row [9..17], etc

rowCells i = [i * 9 .. i * 9 + 8]
colCells i = [i, i + 9 .. i + 72]
boxCells i = map (start +) [0, 1, 2, 9, 10, 11, 18, 19, 20]
             where start = (i `div` 3 * 27) + (i `mod` 3 * 3)
whichRow c = c `div` 9
whichCol c = c `mod` 9
whichBox c = (c `div` 27 * 3) + (c `div` 3 `mod` 3)
relatedCells c = Set.toList $ Set.delete c $ Set.unions $ map Set.fromList xs
    where xs = [rowCells $ whichRow c, colCells $ whichCol c, boxCells $ whichBox c]

-- given a cell, find all digits that haven't yet been used in that cell's
-- row, column, or box
unusedValues :: Int -> [Value] -> [Int]
unusedValues cell nums =
    Set.toList $ Set.difference (Set.fromList [1..9]) (Set.fromList relatedVals)
    where relatedVals = concatMap (flattenVal . (nums !!)) $ relatedCells cell

isSolved = all isKnownVal

solver :: Int -> [Value] -> (Bool, [Value])
solver cell nums = case (nums !! cell, uvals) of
    (IntVal _,   _) | isSolved nums -> (True, nums)
    (UnknownVal, [uval])            -> solver 0 $ inject (IntVal uval)
    _ | cell < 80                   -> solver (succ cell) nums
    _                               -> (False, nums)
    where uvals = unusedValues cell nums
          inject uval = replaceElem cell uval nums

solveGrid :: Grid -> (Bool, Grid)
solveGrid (Grid grid) = (done, Grid grid') where (done, grid') = solver 0 grid

main :: IO ()
main = do
    inStr <- readFile "input.txt"
    let inGrid = Grid $ parseGrid inStr
    let (done, outGrid) = solveGrid inGrid
    putStrLn $ if done
        then "Solved!"
        else "Couldn't quite solve, this is how far I got:"
    putStrLn $ '\n' : show outGrid
