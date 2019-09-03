module Main where

main :: IO ()
main = do
  s <- getLine
  t <- getLine
  print $ sum $ zipWith ((fromEnum.).(==)) s t
