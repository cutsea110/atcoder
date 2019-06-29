module Main where

import Control.Monad
import Data.List

main = do
  n <- readLn
  xs <- sequence $ take n $ repeat getLine
  print $ length $ nub xs
