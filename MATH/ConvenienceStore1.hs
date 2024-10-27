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

solve :: [Integer] -> Int
solve xs = let (a, b, c, d) = classify xs
           in a * d + b * c

classify :: [Integer] -> (Int, Int, Int, Int)
classify xs = foldr f (0, 0, 0, 0) xs
  where
    f 100 (a, b, c, d) = (a+1, b, c, d)
    f 200 (a, b, c, d) = (a, b+1, c, d)
    f 300 (a, b, c, d) = (a, b, c+1, d)
    f 400 (a, b, c, d) = (a, b, c, d+1)
