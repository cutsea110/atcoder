module Main where

import Data.List (unfoldr)
import Data.Tuple (swap)

toDigits :: Int -> [Int]
toDigits = unfoldr phi
  where
    phi n = if n /= 0 then Just (swap (n `divMod` 10)) else Nothing

calc :: [Int] -> Int
calc [n,a,b] = sum $ filter p [1..n]
  where
    p m = a <= s && s <= b where s = sum (toDigits m)

main :: IO ()
main = print . calc =<< fmap read . words <$> getLine
