module Main where

import GHC.Real (toInteger)
import Control.Arrow (first)
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.IntMap as M
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as C

import Debug.Trace (trace)

f $? x = let v = f x
             msg = "{- " ++ show x ++ " -> " ++ show v  ++ " -} "
          in trace msg v

type Parser a = C.ByteString -> Maybe (a, C.ByteString)

parseInt :: Parser Int
parseInt = C.readInt . C.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 cs = case parseInt cs of
  Just (x, cs') -> case parseInt cs' of
    Just (y, cs'') -> Just ((x, y), cs'')
    Nothing -> Nothing
  Nothing -> Nothing

getInts :: IO [Int]
getInts = unfoldr parseInt <$> C.getLine

getIntVec :: Int -> IO (U.Vector Int)
getIntVec n = U.unfoldrN n parseInt <$> C.getLine


getTuple :: IO (Int, Int)
getTuple = do
  (x:y:_) <- getInts
  return (x, y)


main = do
  n:_ <- getInts
  xs  <- getIntVec (fromIntegral n)
  print $ solve n xs

dict :: U.Vector Int -> M.IntMap (Int, Int)
dict xs = U.foldl' f M.empty xs
  where
    f m x | x <= 50000 = M.insertWith (\_ (a, b) -> (a + 1, b    )) x  (1, 0) m
          | otherwise  = M.insertWith (\_ (a, b) -> (a    , b + 1)) x' (0, 1) m
          where x' = 100000 - x

solve :: Int -> U.Vector Int -> Int
solve n xs = M.foldlWithKey f 0 m
  where m = dict xs
        f ttl key (a, b)
          | key == 50000 = comb a 2 + ttl
          | otherwise    = a * b    + ttl

comb :: Int -> Int -> Int
comb n r = product [n-r+1..n] `div` product [1..r]
