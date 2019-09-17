{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Arrow
import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Char
import Data.Function
import Data.Graph
import Data.List
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Builder as BSB
import System.IO (stdout)

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

-- prime means pred
parseInt' :: Parser Int
parseInt' = fmap (pred *** id) . parseInt

getInts :: IO [Int]
getInts = unfoldr parseInt <$> C.getLine

-- prime means pred
getInts' :: IO [Int]
getInts' = unfoldr parseInt' <$> C.getLine

getIntTuple :: IO (Int, Int)
getIntTuple = do
  a:b:_ <- getInts
  return (a, b)

-- prime means pred
getIntTuple' :: IO (Int, Int)
getIntTuple' = do
  a:b:_ <- getInts'
  return (a, b)

getIntTuple3 :: IO (Int, Int, Int)
getIntTuple3 = do
  a:b:c:_ <- getInts
  return (a, b, c)

getIntTuple4 :: IO (Int, Int, Int, Int)
getIntTuple4 = do
  a:b:c:d:_ <- getInts
  return (a, b, c, d)

getIntVec :: Int -> IO (U.Vector Int)
getIntVec n = U.unfoldrN n parseInt <$> C.getLine

-- prime means pred
getIntVec' :: Int -> IO (U.Vector Int)
getIntVec' n = U.unfoldrN n parseInt' <$> C.getLine

---------------------------------------------------------

wins :: Int -> U.Vector Int -> U.Vector Int
wins n is = U.create $ do
  ws <- UM.replicate n 0
  U.forM_ is $ \i -> do
    UM.modify ws (+1) i
  return ws

pr :: Int -> U.Vector Int -> IO ()
pr k ws = do
  U.forM_ ws $ \w -> do
    putStrLn $ if w > k then "Yes" else "No"

main :: IO ()
main = do
  (n,k,q) <- getIntTuple3 :: IO (Int, Int, Int)
  as <- U.replicateM q (fmap pred (readLn :: IO Int))
  let ws = wins n as
  pr (q - k) ws
