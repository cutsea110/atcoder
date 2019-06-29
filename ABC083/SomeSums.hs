module Main where

import Data.List (unfoldr)

toDigits :: Int -> [Int]
toDigits = unfoldr phi
  where
    swap (x,y) = (y,x)
    phi n | n == 0    = Nothing
          | otherwise = Just (swap (n `divMod` 10))

calc :: Int -> Int -> Int -> Int
calc n a b = sum $ filter p [1..n]
  where
    p m = a <= s && s <= b where s = sum (toDigits m)

main = do
  [n,a,b] <- (fmap read . words) <$> getLine
  print (calc n a b)
