module Main where

import GHC.Real (toInteger)
import Control.Arrow (first)
import Data.Char (isSpace)
import Data.List (unfoldr, intercalate)
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
  putStrLn $ intercalate " " $ map show $ primes n

morph :: (Num a, Ord a) => (a -> a) -> (((a, a), [a]) -> [a]) -> a -> [a]
morph f g = u
  where
    u n =  if n < 2 then []
           else let m  = f n
                    ps = u m
                in ps ++ g ((max 2 (m+1), n), ps)

primes :: Integer -> [Integer]
primes 2 = [2]
primes n = morph f g n
  where
    f :: Integer -> Integer
    f n = fromInteger $ floor $ sqrt $ fromInteger n
    g :: ((Integer, Integer), [Integer]) -> [Integer]
    g ((x, y), ps) = filter (\z -> all (\p -> z `mod` p /= 0) ps) [x..y]

