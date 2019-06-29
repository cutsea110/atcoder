module Main where

import Control.Monad (replicateM)
import Data.List (nub)

main :: IO ()
main = do
  n <- readLn
  xs <- replicateM n getLine
  print $ length $ nub xs
