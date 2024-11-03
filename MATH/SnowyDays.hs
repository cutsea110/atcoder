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

getTriple :: IO (Int, Int, Int)
getTriple = do
  (x:y:z:_) <- getInts
  return (x, y, z)

getTuples :: (Monad m, Integral a) => m (a, a) -> a -> m [(a, a)]
getTuples p n = replicateM (fromIntegral n) p

getTriples :: (Monad m, Integral a) => m (a, a, a) -> a -> m [(a, a, a)]
getTriples p n = replicateM (fromIntegral n) p

main = do
  (n, q) <- getTuple
  days <- replicateM q getTriple

  putStrLn $ solve n days

solve :: Int -> [(Int, Int, Int)] -> String
solve n days = unfoldr (psi acc) 1
  where
    psi v i = if i >= n
              then Nothing
              else case signum (v U.! i) of
                1 -> Just ('<', i+1)
                0 -> Just ('=', i+1)
                _ -> Just ('>', i+1)

    acc = U.accum (+) init $ concatMap f days
      where init = U.replicate (n+1) 0
            f (l, r, x) = [(l-1, x), (r, negate x)]
