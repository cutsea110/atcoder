{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Monad
import Data.Maybe
import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString.Char8 as BS

fst3 (x,_,_) = x
snd3 (_,y,_) = y
thd3 (_,_,z) = z

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
  ex <- getIntNList n
  print $ simulate 0 (ini ex)

ini :: [[Int]] -> [(Int, (Int, [Int], State))]
ini xss = zipWith (\x i -> (i, split' x)) xss [1..]

data State = Progress | Keep | Done deriving (Show, Eq, Ord)

step :: [(Int, (Int, [Int], State))] -> [(Int, (Int, [Int], State))]
step ihtbs = zipWith f (V.toList hs') ihtbs
  where
    hs = V.fromList $ map (fst3.snd) ihtbs
    hs' = V.map (\h -> if h == 0 then 0 else hs V.! (h-1)) hs
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
