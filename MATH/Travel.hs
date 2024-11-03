module Main where

import GHC.Real (toInteger)
import Control.Arrow (first)
import Control.Monad (replicateM)
import Data.Char (isSpace)
import Data.List (unfoldr, foldl')
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

getTriple :: IO (Int, Int, Int)
getTriple = do
  (x:y:z:_) <- getInts
  return (x, y, z)

getTuples :: (Monad m, Integral a) => m (a, a) -> a -> m [(a, a)]
getTuples p n = replicateM (fromIntegral n) p

getTriples :: (Monad m, Integral a) => m (a, a, a) -> a -> m [(a, a, a)]
getTriples p n = replicateM (fromIntegral n) p

main = do
  n <- head <$> getInts
  ds <- getIntVec (n-1)
  m <- head <$> getInts
  ss <- replicateM m (head <$> getInts)

  print $ solve ds ss

solve :: U.Vector Int -> [Int] -> Int
solve ds ss = foldl' f 0 ps
  where
    f acc p = acc + distance p
    acc = U.scanl' (+) 0 ds
    ps = zip ss $ tail ss
    distance (s, e) = abs $ (acc U.! pred s) - (acc U.! pred e)
