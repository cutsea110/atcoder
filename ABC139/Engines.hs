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

main :: IO ()
main = do
  !n <- readLn :: IO Int
  !mat <- U.unfoldrN n parseInt2 <$> C.getContents
  print $ sqrt $ fromIntegral $ snd $ calc mat


sortByarg :: [(Int, Int)] -> [(Int, Int)]
sortByarg = sortBy (compare `on` (uncurry atan2 . conv))
  where
    conv = fromIntegral *** fromIntegral

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

ex2 :: [(Int, Int)]
ex2 = [(1,1),(1,0),(0,1),(-1,0),(0,-1)]

ex3 :: [(Int, Int)]
ex3 = [(1,1),(2,2),(3,3),(4,4),(5,5)]

ex4 :: [(Int, Int)]
ex4 = [(90447,91000)]

ex5 :: [(Int, Int)]
ex5 = [(96000, -72000),(-72000, 54000)]

ex6 :: [(Int, Int)]
ex6 = [(1, 2),(3, 4),(5, 6),(7, 8),(9, 10),(11, 12),(13, 14),(15, 16),(17, 18),(19, 20)]


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

----------
{-
3
0 10
5 -5
-5 -5
-}
