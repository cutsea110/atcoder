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
  _  <- getLine
  xs <- getInts
  print $ calcGCD4Integers xs

calcGCD4Integers :: [Integer] -> Integer
calcGCD4Integers = foldl1 calcGCD

calcGCD :: Integer -> Integer -> Integer
calcGCD a b
  | b == 0    = a
  | otherwise = calcGCD b (a `mod` b)
