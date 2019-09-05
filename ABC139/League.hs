module Main where

import Control.Monad (replicateM)
import Data.List (findIndex)

main :: IO ()
main = do
  n <- readLn
  xss <- replicateM n (map read . words <$> getLine) :: IO [[Int]]
  print $ simulate 0 xss

simulate n xs | xs == xs' = -1
              | all null xs' = n+1
              | otherwise = simulate (n+1) xs'
  where xs' = step xs

head' [] = 0
head' (x:xs) = x

tail' [] = []
tail' (_:xs) = xs

step xs = zipWith ($) (matching (map head' xs)) (map tail' xs)

matching :: [Int] -> [[Int] -> [Int]]
matching xs = zipWith f [1..] (map (\i -> if i == 0 then 0 else xs !! (i-1)) xs)
  where
    f i i' | i == i' || i' == 0 = id
           | otherwise = let x' = xs !! (i-1) in (x':)

ex1 :: [[Int]]
ex1 = [[2,3],[1,3],[1,2]]

ex2 :: [[Int]]
ex2 = [[2,3,4],[1,3,4],[4,1,2],[3,1,2]]

ex3 :: [[Int]]
ex3 = [[2,3],[3,1],[1,2]]

-- ex.1
-- 3
-- 2 3
-- 1 3
-- 1 2

-- ex.2
-- 4
-- 2 3 4
-- 1 3 4
-- 4 1 2
-- 3 1 2

-- ex.3
-- 3
-- 2 3
-- 3 1
-- 1 2
