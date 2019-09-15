{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}
module Main where

data WP = Sunny | Cloudy | Rainy deriving (Show, Read, Eq, Ord, Enum)

main :: IO ()
main = do
  !wp <- readLn :: IO WP
  print (next wp)

next :: WP -> WP
next Sunny  = Cloudy
next Cloudy = Rainy
next Rainy  = Sunny
