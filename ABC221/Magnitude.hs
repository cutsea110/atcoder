module Main where

import System.IO

main :: IO ()
main = do
  [a, b] <- map read . words <$> getLine :: IO [Int]
  print $ 32^(a-b)
