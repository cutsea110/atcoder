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
  print $ n^3
