{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad
import Data.Maybe
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as C

fst3 (x,_,_) = x
snd3 (_,y,_) = y
thd3 (_,_,z) = z

tuplify2 (x:y:_) = (x,y)
tuplify2 _ = undefined

--Input functions with ByteString
readInt = fst . fromJust . C.readInt
readIntTuple = tuplify2 . map readInt . C.words
readIntList = map readInt . C.words

getInt = readInt <$> C.getLine
getIntList = readIntList <$> C.getLine
getIntNList n = map readIntList <$> replicateM (fromIntegral n) C.getLine
getIntMatrix = map readIntList . C.lines <$> C.getContents
getIntTuple = readIntTuple <$> C.getLine
getIntNTuples n = map readIntTuple <$> replicateM (fromIntegral n) C.getLine
getIntTuples = map readIntTuple . C.lines <$> C.getContents

main :: IO ()
main = do
  n <- getInt
  ex <- getIntNList n
  print $ simulate 0 (ini ex)

ini :: [[Int]] -> [(Int, (Int, [Int], State))]
ini = zipWith (\i x -> (i, split' x)) [1..]

data State = Progress | Keep | Done deriving (Show, Eq, Ord)

step :: [(Int, (Int, [Int], State))] -> [(Int, (Int, [Int], State))]
step = zipWith f <$> (U.toList . hs) <*> id
  where
    hs = (U.map <$> f <*> id) . U.fromList . map (fst3.snd)
      where f xs h = if h == 0 then 0 else xs U.! (h-1)
    f h' !(i, (h, !t, s)) | h' == i = (i, split' t)
                          | otherwise = (i, (h, t, max Keep s))

simulate :: Int -> [(Int, (Int, [Int], State))] -> Int
simulate n ex | any ((Progress==).thd3.snd) ex' = simulate (n+1) ex'
              | all ((Done==).thd3.snd) ex' = n+1
              | otherwise = -1
  where !ex' = step ex
    

split' :: [Int] -> (Int, [Int], State)
split' [] = (0, [], Done)
split' (x:xs) = (x, xs, Progress)

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
