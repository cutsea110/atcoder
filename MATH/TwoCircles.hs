{-# LANGUAGE BangPatterns #-}
module Main where

import GHC.Real (toInteger)
import Control.Arrow (first)
import Control.Monad (replicateM)
import Data.Char (isSpace)
import Data.Function (on)
import Data.List (unfoldr, foldr)
import Numeric (showFFloat)
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Algorithms.Intro as VA

type Parser a = C.ByteString -> Maybe (a, C.ByteString)

parseInt :: Parser Int
parseInt = C.readInt . C.dropWhile isSpace

parseInteger :: Parser Integer
parseInteger = fmap (first toInteger) . parseInt

getInts :: IO [Int]
getInts = unfoldr parseInt <$> C.getLine

getIntegers :: IO [Integer]
getIntegers = unfoldr parseInteger <$> C.getLine

getIntVec :: Int -> IO (U.Vector Int)
getIntVec n = U.unfoldrN n parseInt <$> C.getLine

getTuple :: IO (Int, Int)
getTuple = do
  (x:y:_) <- getInts
  return (x, y)

getTriple :: IO (Int, Int, Int)
getTriple = do
  (x:y:z:_) <- getInts
  return (x, y, z)

getTuples :: (Monad m, Integral a) => m (a, a) -> a -> m [(a, a)]
getTuples p n = replicateM (fromIntegral n) p

main = do
  p1 <- getTriple
  p2 <- getTriple

  print $ solve p1 p2

solve :: (Int, Int, Int) -> (Int, Int, Int) -> Int
solve (x1, y1, r1) (x2, y2, r2)
  | d <  sub = 1
  | d == sub = 2
  | d >  sub &&
    d <  add = 3
  | d == add = 4
  | d >  add = 5
  where
    !smallr = min r1 r2
    !bigr   = max r1 r2
    !add = (bigr + smallr)^2
    !sub = (bigr - smallr)^2
    !d = (x1-x2)^2+(y1-y2)^2
