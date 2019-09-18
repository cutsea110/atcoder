{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Arrow hiding ((+++))
import Control.Monad
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.Char
import Data.Function
import Data.Graph
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Builder as BSB
import System.IO (stdout)

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x
snd3 :: (a,b,c) -> b
snd3 (_,y,_) = y
thd3 :: (a,b,c) -> c
thd3 (_,_,z) = z

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

-- priority queue
-- https://stackoverflow.com/questions/6976559/comparison-of-priority-queue-implementations-in-haskell
--
data SkewHeap a = Empty
                | SkewNode a (SkewHeap a) (SkewHeap a)
                deriving (Show)

(+++) :: Ord a => SkewHeap a -> SkewHeap a -> SkewHeap a
heap1@(SkewNode x1 l1 r1) +++ heap2@(SkewNode x2 l2 r2) 
  | x1 <= x2   = SkewNode x1 (heap2 +++ r1) l1 
  | otherwise  = SkewNode x2 (heap1 +++ r2) l2
Empty +++ heap = heap
heap +++ Empty = heap

node :: a -> SkewHeap a
node x = SkewNode x Empty Empty

extractMax Empty = Nothing
extractMax (SkewNode x l r ) = Just (x , l +++ r )

fromList :: Ord a => [a] -> SkewHeap a
fromList = foldl' (+++) Empty . map node

foldSkewHeap :: b -> (a -> b -> b -> b) -> SkewHeap a -> b
foldSkewHeap c f = u
  where
    u Empty = c
    u (SkewNode a l r) = f a (u l) (u r)

sumSkewHeap :: SkewHeap Int -> Integer
sumSkewHeap = foldSkewHeap 0 (\a l r -> fromIntegral a + l + r)

---------------------------------------------------------

readProblem :: IO (Int, Int, U.Vector (Int, Int))
readProblem = do
  (n, m) <- getIntTuple
  as <- U.replicateM n getIntTuple
  return (n, m, as)

data T = T { day :: Int, amount :: Int } deriving (Show, Eq)
instance Ord T where
  T f1 s1 <= T f2 s2 = s1 >= s2 || s1 == s2 && f1 <= f2

solve :: (Integer, Map.Map Int (SkewHeap Int)) -> Int -> (Integer, Map.Map Int (SkewHeap Int))
solve prev@(!ttl, !map) !n = maybe prev (\(k, v, hp') -> (ttl+fromIntegral v, Map.alter (const (Just hp')) k map)) mv
  where
    !mv =  Map.foldlWithKey f Nothing map
    f !mv !k = maybe mv (\(v, hp') -> if k <= n && fmap snd3 mv <= Just v then Just (k, v, hp') else mv) . extractMax

solve' :: Map.Map Int (SkewHeap T) -> (Integer, SkewHeap T) -> Int -> (Integer, SkewHeap T)
solve' map (ttl, hp) n = (new_ttl, new_hp)
  where
    mhp = Map.lookup n map
    hp' = maybe hp (hp+++) mhp
    mv = extractMax hp'
    (new_ttl, new_hp) = maybe (ttl, hp') (first ((ttl+).fromIntegral.amount)) mv

main :: IO ()
main = do
  (n, m, as) <- readProblem
  let !map = U.foldl' (\m (k,v) -> Map.insertWithKey (\_ nv ov -> nv +++ ov) k (node (T k v)) m) Map.empty as
  let (!ttl, !hp) = foldl' (solve' map) (0, Empty) [1..m]
  -- print hp
  print ttl
