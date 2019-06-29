module Main where

import Control.Arrow ((***))
import Data.List

sortDesc = sortBy (flip compare)

main :: IO ()
main = do
  n <- readLn
  xs <- map read . words <$> getLine
  let (alice, bob) = (map snd *** map snd) $ partition (odd . fst) $ zip [1..n] $ sortDesc xs
  print $ sum alice - sum bob
