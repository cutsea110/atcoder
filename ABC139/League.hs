{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Arrow
import Control.Monad
import Data.Char
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
  !n <- readLn :: IO Int
  !mat <- U.unfoldrN (n*(n-1)) parseInt <$> C.getContents
  let !ex = V.generate n $ \i -> U.slice (i*(n-1)) (n-1) mat
  let !ex' = ini ex
  print $ simulate 0 ex'

ini :: V.Vector (U.Vector Int) -> (V.Vector (U.Vector Int), U.Vector (Int, Bool))
ini = (fst &&& U.convert . snd) . V.unzip . V.map split

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
              | U.all ((0==).fst) h = n+1
              | U.any ((0/=).fst) h = simulate (n+1) ex'
  where
    !ex'@(!t, !h) = step ex

split :: U.Vector Int -> (U.Vector Int, (Int, Bool))
split xxs = if U.null xxs then (U.empty, (0, False)) else (U.tail xxs, (U.head xxs, True))

type Parser a = C.ByteString -> Maybe (a, C.ByteString)

parseInt :: Parser Int
parseInt = C.readInt . C.dropWhile isSpace

--

ex1 :: V.Vector (U.Vector Int)
ex1 = V.fromList $ map U.fromList [[2,3],[1,3],[1,2]]

ex2 :: V.Vector (U.Vector Int)
ex2 = V.fromList $ map U.fromList [[2,3,4],[1,3,4],[4,1,2],[3,1,2]]

ex3 :: V.Vector (U.Vector Int)
ex3 = V.fromList $ map U.fromList [[2,3],[3,1],[1,2]]

ex4 :: V.Vector (U.Vector Int)
ex4 = V.fromList $ map (\i -> U.fromList $ filter (i/=) ex) ex
  where
    ex = [1..1000]

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
