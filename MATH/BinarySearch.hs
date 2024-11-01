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
  (n, x)  <- getTuple getInts
  xs <- getIntVec n
  -- Problem Title cheating
  putStrLn $ if linearSearch n x xs then "Yes" else "No"

linearSearch :: Int -> Int -> U.Vector Int -> Bool
linearSearch n x xs = go 0
  where
    go !i
      | i == n        = False
      | xs U.! i == x = True
      | otherwise     = go (i+1)
