{-# LANGUAGE BangPatterns #-}
module Main where

import GHC.Real (toInteger)
import Control.Arrow (first)
import Control.Monad (replicateM)
import Data.Char (isSpace)
import Data.List (unfoldr, intercalate)
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

getTupleVec :: Int -> IO (U.Vector (Int, Int))
getTupleVec n = U.replicateM n getTuple

main = do
  !t <- head <$> getInts
  !n <- head <$> getInts
  !ts <- getTuples getTuple n

  U.forM_ (solve t ts) print

solve :: Int -> [(Int, Int)] -> U.Vector Int
solve t ts = U.slice 1 10 $! U.scanl' (+) 0 acc
  where
    !init = U.replicate (t+1) 0
    !acc = U.accum (+) init $ concatMap f ts
      where f (s, e) = [(s, 1), (e, negate 1)]
