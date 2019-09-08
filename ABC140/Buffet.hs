{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Arrow
import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Char
import Data.Function
import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as B

type Parser a = C.ByteString -> Maybe (a, C.ByteString)

parseInt :: Parser Int
parseInt = C.readInt . C.dropWhile isSpace

parseInt2 :: Parser (Int, Int)
parseInt2 = runStateT $
    (,) <$> StateT (C.readInt . C.dropWhile isSpace)
        <*> StateT (C.readInt . B.unsafeTail)
 
parseInt3 :: Parser (Int, Int, Int)
parseInt3 = runStateT $
    (,,) <$> StateT (C.readInt . C.dropWhile isSpace)
        <*> StateT (C.readInt . B.unsafeTail)
        <*> StateT (C.readInt . B.unsafeTail)
 
parseInt4 :: Parser (Int, Int, Int, Int)
parseInt4 = runStateT $
    (,,,) <$> StateT (C.readInt . C.dropWhile isSpace)
        <*> StateT (C.readInt . B.unsafeTail)
        <*> StateT (C.readInt . B.unsafeTail)
        <*> StateT (C.readInt . B.unsafeTail)

main :: IO ()
main = do
  !n <- readLn :: IO Int
  !mat <- U.unfoldrN (3*n-1) parseInt <$> C.getContents
  let as = U.slice 0 n mat
  let bs = U.slice n n mat
  let cs = U.slice (2*n) (n-1) mat
  let bonus = U.zipWith (\i i' -> if i+1 == i' then cs U.! (i-1) else 0) as (U.tail as)
  print $ U.sum bs + U.sum bonus

ex1 :: (U.Vector Int, U.Vector Int, U.Vector Int)
ex1 = (U.fromList [3,1,2], U.fromList [2,5,4], U.fromList [3,6])
