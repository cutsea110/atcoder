{-# LANGUAGE BangPatterns #-}
module Main where

import GHC.Real (toInteger)
import Control.Arrow (first)
import Control.Monad (replicateM)
import Data.Char (isSpace)
import Data.List (unfoldr, foldr)
import Numeric (showFFloat)
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
  (ax, ay)  <- getTuple getInts
  (bx, by)  <- getTuple getInts
  (cx, cy)  <- getTuple getInts

  putStrLn $ showFFloat Nothing (solve (ax, ay) (bx, by) (cx, cy)) ""

cross :: (Int, Int) -> (Int, Int) -> Double
cross (x1, y1) (x2, y2) = fromIntegral $ x1 * y2 - x2 * y1

distance :: (Int, Int) -> (Int, Int) -> Double
distance (x1, y1) (x2, y2) = sqrt $ fromIntegral $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2

solve :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Double
solve a@(ax, ay) b@(bx, by) c@(cx, cy)
  | between = z -- 間にあるなら線分BCに下ろした垂線の長さ
  | otherwise = min x y -- 間にないなら AB か AC の短い方
  where
    x = distance a b
    y = distance a c
    z = abs (cross (ax-cx, ay-cy) (bx-cx, by-cy)) / distance b c
    -- BC の法線ベクトル(A から直線 BC に下ろした垂線のベクトル)
    v = (-(cy-by), cx-bx)
    -- v ベクトル が AB ベクトルと AC ベクトルの間にあるかどうか
    between = signum (cross v (bx-ax, by-ay)) * signum (cross v (cx-ax, cy-ay)) == -1


