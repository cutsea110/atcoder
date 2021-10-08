{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Char
import Data.List
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

main :: IO ()
main = do
  (n, v) <- getProblem'
  print $ (sum [ 2^(j-i-1) | i <- [0..n-1], j <- [i+1..n-1], v U.! i <= v U.! j ]) `mod` largeNum
