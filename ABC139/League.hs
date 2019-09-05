module Main where

import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Char8 as BS

tuplify2 (x:y:_) = (x,y)
tuplify2 _ = undefined

--Input functions with ByteString
readInt = fst . fromJust . BS.readInt
readIntTuple = tuplify2 . map readInt . BS.words
readIntList = map readInt . BS.words

getInt = readInt <$> BS.getLine
getIntList = readIntList <$> BS.getLine
getIntNList n = map readIntList <$> replicateM (fromIntegral n) BS.getLine
getIntMatrix = map readIntList . BS.lines <$> BS.getContents
getIntTuple = readIntTuple <$> BS.getLine
getIntNTuples n = map readIntTuple <$> replicateM (fromIntegral n) BS.getLine
getIntTuples = map readIntTuple . BS.lines <$> BS.getContents

main :: IO ()
main = do
  n <- getInt
  xss <- getIntNList n
  print $ simulate 0 xss

simulate :: Int -> [[Int]] -> Int
simulate n xs | xs == xs' = -1
              | all null xs' = n+1
              | otherwise = simulate (n+1) xs'
  where xs' = step xs

head' :: [Int] -> Int
head' [] = 0
head' (x:xs) = x

tail' :: [Int] -> [Int]
tail' [] = []
tail' (_:xs) = xs

step :: [[Int]] -> [[Int]]
step xs = zipWith ($) (matching (map head' xs)) (map tail' xs)

matching :: [Int] -> [[Int] -> [Int]]
matching xs = zipWith f [1..] (map (\i -> if i == 0 then 0 else xs !! (i-1)) xs)
  where
    f i i' | i == i' || i' == 0 = id
           | otherwise = let x' = xs !! (i-1) in (x':)

{--
ex1 :: [[Int]]
ex1 = [[2,3],[1,3],[1,2]]

ex2 :: [[Int]]
ex2 = [[2,3,4],[1,3,4],[4,1,2],[3,1,2]]

ex3 :: [[Int]]
ex3 = [[2,3],[3,1],[1,2]]
--}

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
