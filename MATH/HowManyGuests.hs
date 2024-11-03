module Main where

import GHC.Real (toInteger)
import Control.Arrow (first)
import Control.Monad (replicateM)
import Data.Char (isSpace)
import Data.Function (on)
import Data.List (unfoldr, foldr, intercalate)
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
  (n, q) <- getTuple
  guests <- getIntVec n
  qs <- getTuples getTuple q

  putStrLn $ intercalate "\n" $ map show $ solve qs guests

solve :: [(Int, Int)] -> U.Vector Int -> [Int]
solve qs xs = map (\(i, j) -> acc U.! j - acc U.! (i-1)) qs
  where
    -- day 0 is convenient for accessing the number of guests at day i
    acc = U.scanl' (+) 0 xs
