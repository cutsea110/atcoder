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

getProblem' :: IO (Int, U.Vector (Int, Int))
getProblem' = do
  n <- readLn :: IO Int
  xs <- U.replicateM n getTuple
  return (n, xs)

getProblem :: IO (Int, [(Int, Int)])
getProblem = do
  n <- readLn :: IO Int
  xs <- replicateM n getTuple
  return (n, xs)

main :: IO ()
main = do
  (n, xs) <- getProblem
  let v = solve n xs
  putStrLn . unwords . map show . tail . V.toList $ solve n xs


solve :: Int -> [(Int, Int)] -> V.Vector Int
solve n = calc n . trans . summary . accumlate . distr

calc :: Int -> [(Int, Int)] -> V.Vector Int
calc n = V.accum (+) (V.replicate (n+1) 0)

trans :: [(Int, Int)] -> [(Int, Int)]
trans xs = zipWith (\(x, c) (y, _) -> (c, y-x)) xs (tail xs)

summary :: [(Int, Int)] -> [(Int, Int)]
summary = map last . groupBy (\(i,_) (j,_) -> i == j)

accumlate :: [(Int, Int)] -> [(Int, Int)]
accumlate = snd . mapAccumL f 0
  where f cnt (i, c) = let c' = cnt+c in (c', (i, c'))

distr :: [(Int, Int)] -> [(Int, Int)]
distr = sort . foldr f [] -- insert sort is O(n^2)
  where f (a, b) acc = (a, 1):(a+b, -1):acc
