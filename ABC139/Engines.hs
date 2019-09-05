{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Arrow
import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Char
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as B

main :: IO ()
main = do
  !n <- readLn :: IO Int
  !mat <- U.unfoldrN n parseInt2 <$> C.getContents
  -- let !ex = V.generate n $ \i -> U.slice (i*2) 2 mat
  print $ sqrt $ fromIntegral $ snd $ calc mat

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

calc :: U.Vector (Int, Int) -> ((Int, Int), Int)
calc = U.foldl f ((0,0), 0)
  where
--    f :: Num a => ((a, a), a) -> (a, a) -> ((a, a), a)
    f cs@((cx, cy), ttl) (mx, my) | ttl' >= ttl = ((cx', cy'), ttl')
                                  | otherwise   = cs
      where
        (cx', cy') = (cx+mx, cy+my)
        ttl' = cx'^2 + cy'^2

ex1 :: [(Int, Int)]
ex1 = [(0,10),(5,-5),(-5,-5)]

----------
{-
3
0 10
5 -5
-5 -5
-}
