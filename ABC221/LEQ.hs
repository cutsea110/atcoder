{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Debug.Trace (trace)

import Control.Arrow
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Char
import Data.Function (on)
import Data.List
import Data.STRef
import qualified Data.Tree as T
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Intro as Intro
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.ByteString.Char8 as C

extEuclid :: Integral a => a -> a -> (a, (a, a))
extEuclid x y = psi x y (1, 0) (0, 1)
  where psi x 0 p@(!p1, !p2) _            = (x, p)
        psi x y p@(!p1, !p2) q@(!q1, !q2) = psi y m q (p |-| d |*| q)
          where (d, m) = x `divMod` y

(|*|) :: Integral a => a -> (a, a) -> (a, a)
s |*| (x, y) = (s*x, s*y)
infixl 7 |*|

(|-|) :: Integral a => (a, a) -> (a, a) -> (a, a)
(x1, y1) |-| (x2, y2) = (x1-x2, y1-y2)
infixl 6 |-|

{- |
-- mod 13 における (2^3) の逆数 1/(2^3) を求める
>>> modInv 13 (2^3)
5
-- 5 が (2~3) の逆数なので (2^3) と書ければ 1 になる
>>> (2^3) * 5 `mod` 13
1
-}
modInv :: Integral a => a -> a -> a
modInv g p = (x+g) `mod` g
  where (_, (_, x)) = extEuclid g p

modPower :: Integral a => a -> a -> a -> a
modPower g x p
  | p == 0 = 1
  | p == 1 = x `mod` g
  | otherwise = x' * x' `mod` g * k `mod` g
  where (d, m) = p `divMod` 2
        x' = modPower g x d
        k = if m == 0 then 1 else x

type Parser a = C.ByteString -> Maybe (a, C.ByteString)

parseInt :: Parser Int
parseInt = C.readInt . C.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 cs = case parseInt cs of
  Just (x, cs') -> case parseInt cs' of
    Just (y, cs'') -> Just ((x, y), cs'')
    Nothing -> Nothing
  Nothing -> Nothing

getInts :: IO [Int]
getInts = unfoldr parseInt <$> C.getLine

getIntVec :: Int -> IO (U.Vector Int)
getIntVec n = U.unfoldrN n parseInt <$> C.getLine

getTuple :: IO (Int, Int)
getTuple = do
  (x:y:_) <- getInts
  return (x, y)

------------------------------------------------
-- Main
------------------------------------------------

const_MODULO = 998244353

getProblem' :: IO (Int, U.Vector Int)
getProblem' = do
  n <- readLn :: IO Int
  xs <- getIntVec n
  return (n, xs)

getProblem :: IO (Int, [Int])
getProblem = do
  n <- readLn :: IO Int
  xs <- getInts
  return (n, xs)

-- main :: IO ()
main = do
  (_, v) <- getProblem'
  let (n, xs) = monotone v
  print $ solve n xs

---------------------------------------------------
-- BIT の (<>) も mod計算する
instance Semigroup Int where
  x <> y = (x + y) `mod` const_MODULO

instance Monoid Int where
  mempty = 0
---------------------------------------------------

solve :: Int -> U.Vector Int -> Int
solve maxVal v = runST $ do
  let n = U.length v
  let (modInv', modPow') = (modInv const_MODULO, modPower const_MODULO)

  wk <- new maxVal
  result <- newSTRef 0

  forM_ [0..n - 1] $ \i -> do
    let a = v U.! i
    w <- sumTo wk a
    modifySTRef' result (modPow' 2 i * w <>)
    inc a (modInv' (modPow' 2 (i+1))) wk

  readSTRef result

{- | 単調写像により 1-origin な整数に番号を振り直して、最大値も同時に返す
-}
monotone :: (Num a, Ord a, UM.Unbox a) => U.Vector a -> (a, U.Vector a)
monotone vs = runST $ do
  let indexed = U.indexed vs
  let sorted = U.modify (Intro.sortBy (compare `on` snd)) indexed
  ss <- U.thaw sorted
  (newMax, _origMax) <- comp ss (UM.length ss) 0 (0, 0)
  ss' <- U.freeze ss
  let restored = U.modify (Intro.sortBy (compare `on` fst)) ss'
  return (newMax, U.map snd restored)
  where
    {- | 前提: v は snd について sort 済とする-}
    comp vec n i (ix, x) = do
      if i > n-1 then return (ix, x) -- (振りなおした最大値, オリジナルの最大値)
        else do
        (j, y) <- UM.read vec i
        let (ix', x') = if y > x then (ix+1, y) else (ix, x)
        UM.modify vec (second $ const ix') i
        comp vec n (i+1) (ix', x')


---------------------------------------------
-- Binary Indexed Tree
---------------------------------------------

{- |
1-base です(0 は使用しない)
-}
new :: (PrimMonad m, Monoid a, UM.Unbox a)
    => Int -> m (UM.MVector (PrimState m) a)
new n = do
  UM.replicate (n+1) mempty

{- |
i に w を加算
-}
inc :: (PrimMonad m, Monoid a, UM.Unbox a)
    => Int -> a -> UM.MVector (PrimState m) a -> m ()
inc i w v = do
  let n = UM.length v - 1 -- 最大値
  if i > n then return ()
    else do
    UM.modify v (<>w) i
    inc (i + (i .&. negate i)) w v

{- |
i までの総和
-}
sumTo :: (PrimMonad m, Monoid a, UM.Unbox a)
      => UM.MVector (PrimState m) a -> Int -> m a
sumTo v i = do
  if i < 1 then return mempty
    else do
    x <- UM.read v i
    y <- sumTo v (i - (i .&. negate i))
    return (x <> y)
