{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveFunctor #-}
module Main where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Bits
import Data.Char
import Data.List
import qualified Data.Tree as T
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector.Generic.Mutable as M
import qualified Data.ByteString.Char8 as C

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

largeNum = 998244353

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
  (n, v) <- getProblem
  let vs = V.fromList $ inverse v
  let xs = [ sum xs `mod` largeNum | j <- [0..n-1], let xs = map (\i -> 2^(j-i-1)) (snd (vs V.! j)) ]
  print $ sum  xs `mod` largeNum
  -- print $ (sum [ 2^(j-i-1) | i <- [0..n-1], j <- [i+1..n-1], v U.! i <= v U.! j ]) `mod` largeNum

inverse :: [Int] -> [(Int, [Int])]
inverse xs = go (zip [0..] xs) t []
  where
    bw = bitWidth (10^9+10)
    t = new bw
    go []             t acc = zip xs (reverse acc)
    go (ix@(i, x):xs) t acc = let acc' = ({- t ! (2^bw-1) \\ -} t ! x) : acc
                                  t' = inc x [i] t -- Monoid は [a]
                              in go xs t' acc'

-- UTILITY

bitWidth :: Int -> Int
bitWidth = length . toBit

toBit :: Int -> [Int]
toBit = reverse . unfoldr psi
  where
    psi n | n == 0 = Nothing
          | otherwise = let (q, r) = n `divMod` 2
                        in Just (r, q)
-- Bibary Indexed Tree

data BIT a = BIT Int (Tree a) deriving (Show, Functor)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Functor)

toForest :: Tree a -> T.Forest a
toForest Empty = []
toForest (Node x l r) = [T.Node x (toForest l ++ toForest r)]

drawBIT :: Show a => BIT a -> IO ()
drawBIT (BIT k t) = do
  putStrLn (show k ++ " bits")
  putStr . T.drawForest . fmap (fmap show) . toForest $ t

{- |
construct BIT on k bits. O(n)
-}
new :: Monoid a => Int -> BIT a
new k = BIT k $ f k
  where f 0 = Empty
        f k = Node mempty fk' fk' where fk' = f (k-1)

{- |
construct BIT by maximum value n.
-}
newByMax :: Monoid a => Int -> BIT a
newByMax n = new k
  where k = bitWidth n

{- |
Increment the value at index i(1-base) by amount x. O(log n)
-}
inc :: Monoid a => Int -> a -> BIT a -> BIT a
inc i x (BIT k root) = BIT k $ f root (k-1) 0
  where f Empty _ _ = error "invalid arguments"
        f (Node y l r) j acc
          | i `testBit` j =
              if acc' == i
                  then  y'   `seq` Node y' l r
                  else  acc' `seq` Node y (f l j' acc') r
          | otherwise = y'   `seq` Node y' l (f r j' acc)
          -- NOTE: If y `mappend` x for List Monoid, then we should reverse to return value at lookup by (!).
          where y' = x `mappend` y
                j' = j-1
                acc' = acc `setBit` j

{- |
Lookup the sum of all values from index 1 to index i. O(log n)
-}
(!) :: Monoid a => BIT a -> Int -> a
BIT k root ! i = f root (k-1) mempty
  where f Empty _ acc = acc
        f (Node x l r) j acc
          | i `testBit` j = acc' `seq` f l j' acc'
          | otherwise     = f r j' acc
          where j'   = j-1
                acc' = acc `mappend` x


-----------------------------------------------

