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

neighbors :: Int -> U.Vector (Int, Int) -> V.Vector [Int]
neighbors n es = V.create $ do
  vec <- VM.replicate n []
  U.forM_ es $ \(a, b) -> do
    VM.modify vec (b:) a
    VM.modify vec (a:) b
  return vec

values :: Int -> U.Vector (Int, Int) -> U.Vector Int
values n ops = U.create $ do
  vec <- UM.replicate n 0
  U.forM_ ops $ \(p, pt) -> do
    UM.modify vec (+pt) p
  return vec

accumulate :: Int -> V.Vector [Int] -> U.Vector Int -> U.Vector Int
accumulate n ns vs = U.create $ do
  vec <- UM.new n
  let dfs !p !q !acc = do
        let acc' = acc + vs U.! q
        UM.write vec q acc'
        forM_ (ns V.! q) $ \i -> do
          when (i /= p) $ do
            dfs q i acc'
  dfs (-1) 0 0
  return vec

main :: IO ()
main = do
  (n, q) <- getIntTuple
  es <- U.replicateM (n-1) getIntTuple'
  ops <- U.replicateM q (fmap (pred *** id) getIntTuple)
  print (es, ops)
  let ns = neighbors n es
  print ns
  let vs = values n ops
  print vs
  let res = accumulate n ns vs
  print res
  
--  let !g = genTree n a
--  let !ans = U.map (\i -> U.foldr (\(i', pt) ttl -> if path g (i'-1) i then pt+ttl else ttl) 0 q) $ U.enumFromN 0 n
--  putStrLn $ intercalate " " (map show (U.toList ans))

genTree :: Int -> U.Vector (Int, Int) -> Graph
genTree n ps = let (!g, _, _) = graphFromEdges (V.toList ns) in g
  where
    cs :: V.Vector (U.Vector Int)
    !cs = V.generate n $ \i -> U.map snd (U.filter ((==(i+1)).fst) ps)
    ns :: V.Vector (Int, Int, [Int])
    !ns = V.zipWith (\xs i -> (i, i+1, U.toList xs)) cs (V.enumFromN 0 n)

ex1 :: (U.Vector (Int, Int), U.Vector (Int, Int))
ex1 = (U.fromList [(1,2),(2,3),(2,4)], U.fromList [(2,10),(1,100),(3,1)])
