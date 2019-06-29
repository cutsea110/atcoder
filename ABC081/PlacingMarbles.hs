module PlacingMarbles where

import Data.Char (ord)

main :: IO ()
main = do
  xs <- getLine
  print $ foldr (\c s -> (ord c - ord '0') + s) 0 xs
