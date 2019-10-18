-- Informatics 1 - Introduction to Computation
-- Functional Programming Tutorial 5
--
-- Week 5(14-18 Oct.)

module Tutorial5 where

import Data.Char
import Data.Ratio
-- import Test.QuickCheck

-- 1. Map

-- a.
doubles :: [Int] -> [Int]
doubles xs = map (\x -> x*2) xs

-- b.        
penceToPounds :: [Int] -> [Float]
penceToPounds pencexs = map (\x -> fromIntegral x / 100) pencexs

-- c.
uppersComp :: String -> String
uppersComp str = map toUpper str


-- 2. Filter
-- a.
alphas :: String -> String
alphas str = filter isAlpha str

-- b.
above :: Int -> [Int] -> [Int]
above threshold xs = filter (\x -> x > threshold) xs

-- c.
unequals :: [(Int,Int)] -> [(Int,Int)]
unequals xs = filter (\a -> fst a /= snd a) xs

-- d.
rmCharComp :: Char -> String -> String
rmCharComp char str = filter (\c -> char /= c) str


-- 3. Comprehensions vs. map & filter
-- a.
largeDoubles :: [Int] -> [Int]
largeDoubles xs = [2 * x | x <- xs, x > 3]

largeDoubles' :: [Int] -> [Int]
largeDoubles' xs = map (\x -> 2 * x) (filter (\k -> k > 3) xs)

prop_largeDoubles :: [Int] -> Bool
prop_largeDoubles xs = largeDoubles xs == largeDoubles' xs 

-- b.
reverseEven :: [String] -> [String]
reverseEven strs = [reverse s | s <- strs, even (length s)]

reverseEven' :: [String] -> [String]
reverseEven' strs = map reverse (filter (\s -> even (length s)) strs)

prop_reverseEven :: [String] -> Bool
prop_reverseEven strs = reverseEven strs == reverseEven' strs



-- 4. Foldr
-- a.
andRec :: [Bool] -> Bool
andRec []     = True
andRec (x:xs) = x && andRec xs

andFold :: [Bool] -> Bool
andFold bools = foldr (&&) True bools

prop_and :: [Bool] -> Bool
prop_and xs = andRec xs == andFold xs

-- b.
concatRec :: [[a]] -> [a]
concatRec [] = []
concatRec (x:xs) = x ++ concatRec xs

concatFold :: [[a]] -> [a]
concatFold xs = foldr (++) [] xs

prop_concat :: [String] -> Bool
prop_concat strs = concatRec strs == concatFold strs

-- c.
rmCharsRec :: String -> String -> String
rmCharsRec _ [] = []
rmCharsRec rmv (x:xs)
    | elem x rmv = rmCharsRec rmv (rmCharComp x xs)
    | otherwise = x : rmCharsRec rmv xs

rmCharsFold :: String -> String -> String
-- rmCharsFold rmv (x:xs) = foldr (\c, acc -> if (elem c rmv) then (rmCharComp c xs)++acc else (x:xs)++acc)  (x:xs) 
rmCharsFold rmv str = undefined

prop_rmChars :: String -> String -> Bool
prop_rmChars chars str = rmCharsRec chars str == rmCharsFold chars str


type Matrix = [[Rational]]

-- 5
-- a.
uniform :: [Int] -> Bool
uniform xs = all (\x -> x == head xs) xs

-- b. rows are all of equal length and at least one row and one column
valid :: Matrix -> Bool
valid m = (uniform [length row | row <- m]) && length m >= 1 && length (head m) >= 1


-- 6.
matrixWidth :: Matrix -> Int
matrixWidth m = length (head m)

matrixHeight :: Matrix -> Int
matrixHeight m = length m

plusM :: Matrix -> Matrix -> Matrix
plusM m1 m2
    | (matrixWidth m1 /= matrixWidth m2) || (matrixHeight m1 /= matrixHeight m2) = error "not applicable"
	

-- 7.
timesM :: Matrix -> Matrix -> Matrix
timesM = undefined

-- 8.
-- b.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = undefined

-- c.
zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith'' f xs ys = undefined

-- -----------------------------------
-- -----------------------------------
-- -- Optional material
-- -----------------------------------
-- -----------------------------------
-- -- 9.

-- Mapping functions
mapMatrix :: (a -> b) -> [[a]] -> [[b]]
mapMatrix f = undefined

zipMatrix :: (a -> b -> c) -> [[a]] -> [[b]] -> [[c]]
zipMatrix f = undefined

-- All ways of deleting a single element from a list
removes :: [a] -> [[a]]     
removes = undefined

-- Produce a matrix of minors from a given matrix
minors :: Matrix -> [[Matrix]]
minors m = undefined

-- A matrix where element a_ij = (-1)^(i + j)
signMatrix :: Int -> Int -> Matrix
signMatrix w h = undefined
        
determinant :: Matrix -> Rational
determinant = undefined

cofactors :: Matrix -> Matrix
cofactors m = undefined        
                
scaleMatrix :: Rational -> Matrix -> Matrix
scaleMatrix k = undefined

inverse :: Matrix -> Matrix
inverse m = undefined

-- Tests
identity :: Int -> Matrix
identity n = undefined

-- prop_inverse2 :: Rational -> Rational -> Rational 
                -- -> Rational -> Property
-- prop_inverse2 a b c d = undefined

type Triple a = (a,a,a)
        
-- prop_inverse3 :: Triple Rational -> 
                 -- Triple Rational -> 
                 -- Triple Rational ->
                 -- Property
prop_inverse3 r1 r2 r3 = undefined