{-# LANGUAGE BangPatterns #-}
module Main where

import Debug.Trace (trace)

import GHC.Real (toInteger)
import Control.Arrow (first)
import Control.Monad (replicateM)
import Data.Char (isSpace)
import Data.List (unfoldr, foldr)
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as U

f $? x = let v = f x
             msg = "{- " ++ show x ++ " => " ++ show v ++ "-}"
         in trace msg v

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

getTuple :: MonadFail m => m [a] -> m (a, a)
getTuple p = do
  (x:y:_) <- p
  return (x, y)

getTuples :: (Monad m, Integral a) => m (a, a) -> a -> m [(a, a)]
getTuples p n = replicateM (fromIntegral n) p

main = do
  n  <- head <$> getInts
  hs <- getInts

  print $ solve hs

extract :: [Int] -> Int
extract (x:y:_) = max x y

solve :: [Int] -> Int
solve = extract . foldr f []
  where
    f x []  = [x]
    f x [y] = [x, y]
    f x [!y1, y0] = [x+y0, y1, y0]
    f x [!y2, y1, y0] = [max (x+y1) (x+y0), y2, y1]
