{-# LANGUAGE BangPatterns,
             TupleSections
#-}
module Main where

import GHC.Real (toInteger)
import Control.Arrow (first)
import Data.Char (isSpace)
import Data.List (unfoldr, intercalate, foldl')
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as C

type Parser a = C.ByteString -> Maybe (a, C.ByteString)

parseInt :: Parser Integer
parseInt = fmap (first toInteger) . C.readInt . C.dropWhile isSpace

parseInt2 :: Parser (Integer, Integer)
parseInt2 cs = case parseInt cs of
  Just (x, cs') -> case parseInt cs' of
    Just (y, cs'') -> Just ((x, y), cs'')
    Nothing -> Nothing
  Nothing -> Nothing

getInts :: IO [Integer]
getInts = unfoldr parseInt <$> C.getLine

getTuple :: IO (Integer, Integer)
getTuple = do
  (x:y:_) <- getInts
  return (x, y)


main = do
  n:_ <- getInts
  print $ solve (fromIntegral n)

-- solve :: Int -> Integer
solve n = U.ifoldl' (\ttl i e -> ttl +  i * e) 0 vec
  where
    vec = U.accum (+) init update
    init = U.replicate (n+1) 1
    update = concatMap f [2..n]
      where f i = map (,1::Int) [i, i+i..n]
