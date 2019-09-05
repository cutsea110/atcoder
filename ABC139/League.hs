module Main where

import Control.Monad
import Data.Maybe
import qualified Data.Vector.Unboxed as V
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
  ex <- getIntNList n
  print $ simulate 0 (ini ex)

ini :: [[Int]] -> [(Int, (Int, [Int]))]
ini xss = zipWith (\x i -> (i, (head' x, tail' x))) xss [1..]

step :: [(Int, (Int, [Int]))] -> [(Int, (Int, [Int]))]
step ihts = bs
  where
    hs = V.fromList $ map (fst.snd) ihts
    hs' = V.map (\h -> if h == 0 then 0 else hs V.! (h-1)) hs
    bs = zipWith (\h' (i, (h, t)) -> if h' == i then (i, (head' t, tail' t)) else (i, (h, t))) (V.toList hs') ihts

simulate :: Int -> [(Int, (Int, [Int]))] -> Int
simulate n ex | ex == ex' = -1
              | all (((0,[])==).snd) ex' = n+1
              | otherwise = simulate (n+1) ex'
  where ex' = step ex

head' :: [Int] -> Int
head' [] = 0
head' (x:xs) = x

tail' :: [Int] -> [Int]
tail' [] = []
tail' (_:xs) = xs

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
