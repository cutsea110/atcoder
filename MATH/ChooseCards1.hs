module Main where

import GHC.Real (toInteger)
import Control.Arrow (first)
import Data.Char (isSpace)
import Data.List (unfoldr)
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
  n <- getLine
  xs <- getInts
  print $ solve xs

solve :: [Integer] -> Integer
solve xs = let (a, b, c) = classify xs
           in comb2 a + comb2 b + comb2 c


comb2 :: Integer -> Integer
comb2 n = n * (n-1) `div` 2

classify :: [Integer] -> (Integer, Integer, Integer)
classify xs = foldr f (0, 0, 0) xs
  where
    f 1 (a, b, c) = (a+1, b, c)
    f 2 (a, b, c) = (a, b+1, c)
    f 3 (a, b, c) = (a, b, c+1)

