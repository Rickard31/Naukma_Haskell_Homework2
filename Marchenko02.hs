{-# OPTIONS_GHC -Wall #-}
module Marchenko02 where

-- ������ 1 -----------------------------------------
sumFl :: [Integer] -> Integer
sumFl  = foldl (+) 0
  
-- ������ 2 ----------------------------------------- 
productFr :: [Integer] -> Integer
productFr  = foldr (*) 1

-- ������ 3 -----------------------------------------
concatFr :: [Int] -> [Int] -> [Int]
concatFr xs ys = foldr (:) ys xs 


-- ������ 4 -----------------------------------------
insert :: [Int] -> Int -> [Int]
insert [] v = [v]
insert xs v = if v<=head xs then v:xs else head xs : insert (tail xs) v

sortInsert :: [Int] -> [Int]
sortInsert  = foldl insert []

-- ������ 5 -----------------------------------------
findIndices ::(Int -> Bool) -> [Int] -> [Int] 
findIndices predicate xs = [i | i <- [0..length (xs) - 1], predicate (xs!!i)]

-- ������ 6 -----------------------------------------
allReverse :: [String] -> [String]
allReverse xs = map reverse (reverse xs)

-- ������ 7  -----------------------------------------
isDigit :: Char -> Bool
isDigit ch = not (elem ch ['1', '2', '3', '4', '5', '6', '7', '8', '9', '0'])

noDigits :: String -> String
noDigits = filter isDigit

-- ������ 8 ------------------------------------------
cntGood :: [Int -> Bool] -> Int -> Int
cntGood ps v = length (filter ($ v) ps)

-- ������ 9 ------------------------------------------

nextStep :: [Integer] -> [Integer]
nextStep xs = [xs!!0] ++ [xs!!i + xs!!(i+1) | i <- [0..length(xs)-2]] ++ [last xs]

trianglePas :: [[Integer]]
trianglePas = iterate nextStep [1]

-- ������ 10 -----------------------------------------
factorialsM :: [Integer]
factorialsM = 1 : zipWith (*) factorialsM [2..]

