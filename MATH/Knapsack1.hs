{-# LANGUAGE BangPatterns #-}
module Main where

import GHC.Real (toInteger)
import Control.Arrow (first)
import Control.Monad (replicateM)
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as U

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

getTuples :: (Monad m, Integral a) => m (a, a) -> a -> m [(a, a)]
getTuples p n = replicateM (fromIntegral n) p

main = do
  (!n, !w) <- getTuple
  items <- U.replicateM n getTuple

  print $ solve (n, w) items

type Weight = Int
type Value = Int

solve :: (Int, Weight) -> U.Vector (Weight, Value) -> Int
solve (n, w) = U.head . U.foldr f (U.replicate (w+1) 0)
  where
    f (wi, vi) mem = U.zipWith max mem mem'
      where
        mem' = U.map (+vi) (U.drop wi mem) U.++ U.replicate wi 0
