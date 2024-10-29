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

getTuple :: MonadFail m => m [a] -> m (a, a)
getTuple p = do
  (x:y:_) <- p
  return (x, y)

getTuples :: (Monad m, Integral a) => m (a, a) -> a -> m [(a, a)]
getTuples p n = replicateM (fromIntegral n) p

main = do
  n  <- head <$> getInts
  hs <- getInts

  print $ solve' hs

type Pos = Int
type Pt = Int

solve' :: [Pos] -> Pt
solve' = extract . solve
  where
    extract ((pos1, pt1), (pos0, pt0)) = min pt1 (pt0 + abs (pos1 - pos0))

solve :: [Pos] -> ((Pos, Pt), (Pos, Pt))
solve = foldr2 f g
  where
    f pos1 pos0 = ((pos1, abs $ pos1 - pos0), (pos0, 0))
    g pos2 (prev@(pos1, !pt1), (pos0, !pt0)) = ((pos2, pt), prev)
      where !pt = min (pt1 + abs (pos2 - pos1)) (pt0 + abs (pos2 - pos0))

foldr2 :: (a -> a -> b) -> (a -> b -> b) -> [a] -> b
foldr2 f g = u
  where
    u [x1, x0] = f x1 x0
    u (x:xs)   = g x (u xs)
