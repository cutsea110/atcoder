{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Char (isSpace)
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as C

type Parser a = C.ByteString -> Maybe (a, C.ByteString)

parseInt :: Parser Int
parseInt = C.readInt . C.dropWhile isSpace

getIntVec :: Int -> IO (U.Vector Int)
getIntVec n = U.unfoldrN n parseInt <$> C.getLine

foldn (c, f) 0 = c
foldn (c, f) n = f (foldn (c, f) (n-1))

paran (c, f) 0 = c
paran (c, f) n = f n (paran (c, f) (n-1))

---------------------------------------------------------

frog1 vec = paran (step vec 0 undefined, step vec)

step vec 0 _ = (0, (vec U.! 0, undefined, undefined, undefined, undefined))
step vec 1 (c, (h, h1, c1, h2, c2)) = (c', (h', h, c, h1, c1))
  where
    h' = vec U.! 1
    c' = abs (h'-h)
step vec i (c, (h, h1, c1, h2, c2)) = (c', (h', h1', c1', h2', c2'))
  where
    h' = vec U.! i
    h1' = h
    c1' = c
    h2' = h1
    c2' = c1
    d1 = abs (h'-h1')
    d2 = abs (h'-h2')
    c' = min (c1'+d1) (c2'+d2)

main :: IO ()
main = do
  !n <- readLn :: IO Int
  !hs <- getIntVec n
  print $ fst $ frog1 hs (n-1)
