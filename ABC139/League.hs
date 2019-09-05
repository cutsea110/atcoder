{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Arrow
import Control.Monad
import Data.Maybe
import qualified Data.Vector as V
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
  !n <- getInt
  !ex <- getIntNList n
  print $ simulate 0 (ini ex)

ini :: [[Int]] -> (V.Vector (U.Vector Int), U.Vector (Int, Bool))
ini = (fst &&& U.convert . snd) . V.unzip . V.fromList . map (split . U.fromList)

step :: (V.Vector (U.Vector Int), U.Vector (Int, Bool)) -> (V.Vector (U.Vector Int), U.Vector (Int, Bool))
step (!ts, !hs) = (fst &&& U.convert . snd) $ V.unzip $ V.zipWith f (V.convert h'h) ts
  where
     !h'h = U.map g (U.indexed hs)
       where
         g (!i, (!h, !b)) = let h' = fst (hs U.! (h-1))
                            in if h == 0 || i+1 == h' then (0,h) else (h',h)
     f (!h', !h) !t = if h' == 0 then (xs, (x, True)) else (t, (h, False))
       where
         (!x, !xs) = if U.null t then (0, t) else (U.head t, U.tail t)

simulate :: Int -> (V.Vector (U.Vector Int), U.Vector (Int, Bool)) -> Int
simulate n ex | U.all (not.snd) h = -1
               | U.any ((0/=).fst) h = simulate (n+1) ex'
               | U.all ((0==).fst) h = n+1
  where
    !ex'@(!t, !h) = step ex

split :: U.Vector Int -> (U.Vector Int, (Int, Bool))
split xxs = if U.null xxs then (U.empty, (0, False)) else (U.tail xxs, (U.head xxs, True))

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
