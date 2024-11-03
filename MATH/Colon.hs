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
  a:b:h:m:_ <- getInts

  print $ solve (a, b) (h, m)

solve :: (Int, Int) -> (Int, Int) -> Double
solve (a, b) (h, m)
  = sqrt $ fromIntegral a^2 + fromIntegral b^2 - 2 * fromIntegral a * fromIntegral b * cos theta
  where
    !minutes = fromIntegral $ h * 60 + m
    !angleMin  = fromIntegral (6 * minutes `mod` 360)
    !angleHour = fromIntegral (5 * minutes `mod` 3600) / 10 -- means 0.5 * minutes `mod` 360
    !theta = 2 * pi * abs (angleMin - angleHour) / 360
