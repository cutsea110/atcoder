module Main where

import Text.Printf (printf)

pat :: Int -> Int -> [(Int, Int, Int)]
pat n y = [(a,b,c)| a <- [0..n], b <- [0..n-a], let c = n - a - b, 10000*a+5000*b+1000*c == y]

main :: IO ()
main = do
  [n,y] <- (map read . words) <$> getLine
  let ps = pat n y
  let (a,b,c) = if null ps then (-1,-1,-1) else head ps
  printf "%d %d %d\n" a b c
