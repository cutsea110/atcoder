module Main where

import GHC.Real (toInteger)
import Control.Arrow (first)
import Data.Char (isSpace)
import Data.List (unfoldr, find)
import qualified Data.ByteString.Char8 as C

type Parser a = C.ByteString -> Maybe (a, C.ByteString)

parseInt :: Parser Integer
parseInt = fmap (first toInteger) . C.readInt . C.dropWhile isSpace

parseInt2 :: Parser (Integer, Integer)
parseInt2 cs = do
  (x, cs')  <- parseInt cs
  (y, cs'') <- parseInt cs'
  return ((x, y), cs'')

getInts :: IO [Integer]
getInts = unfoldr parseInt <$> C.getLine

getTuple :: IO (Integer, Integer)
getTuple = do
  (x:y:_) <- getInts
  return (x, y)


main = do
  (n:s:_) <- getInts
  xs      <- getInts
  putStrLn $ solve s xs

-- solve :: Integer -> [Integer] -> Integer
solve s xs = maybe "No" (const "Yes") $ find (==s) $ foldl f [0] xs
  where
    f zs y = filter (<=s) $ zs ++ map (+y) zs
