module Main where

import Control.Monad (replicateM)

calc :: [Int] -> Int
calc [a, b, c, x] = sum [1| a'<-[0..a], b'<-[0..b], c'<-[0..c], 500*a'+100*b'+50*c' == x]

main :: IO ()
main = print . calc =<< replicateM 4 readLn
