module Main where

import Text.Printf (printf)

main :: IO ()
main = do
  (a, [b,c], s) <- (,,) <$> readLn <*> fmap (map read . words) getLine <*> getLine
  printf "%d %s\n" (a+b+c :: Int) s
