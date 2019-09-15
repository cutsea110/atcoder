{-# LANGUAGE BangPatterns #-}
module Main where

easyOdd :: String -> Bool
easyOdd []      = True
easyOdd ('R':s) = easyEven s
easyOdd ('U':s) = easyEven s
easyOdd ('D':s) = easyEven s
easyOdd _       = False

easyEven :: String -> Bool
easyEven []      = True
easyEven ('L':s) = easyOdd s
easyEven ('U':s) = easyOdd s
easyEven ('D':s) = easyOdd s
easyEven _       = False

main :: IO ()
main = do
  !s <- getLine
  putStrLn $ if easyOdd s then "Yes" else "No"
