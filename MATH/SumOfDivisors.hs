module Main where

import Data.Char (isSpace)
import Data.List (unfoldr, foldl')
import qualified Data.ByteString.Char8 as C

type Parser a = C.ByteString -> Maybe (a, C.ByteString)

parseInt :: Parser Int
parseInt = C.readInt . C.dropWhile isSpace

getInts :: IO [Int]
getInts = unfoldr parseInt <$> C.getLine

main = do
  n:_ <- getInts
  print $ solve n

solve :: Int -> Int
solve n = foldl' f (n * succ n `div` 2) [2..n]
  where
    f acc i = acc + sum [i,i+i..n] -- for 1th
