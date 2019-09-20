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

pair (f, g) x = (f x, g x)
cross (f, g) (x, y) = (f x, g y)
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

-- Fixpoint
newtype Fix f = In { out :: f (Fix f) }
newtype HisF f a = His { unHis :: (a, f (HisF f a)) }
data Hisx f a x = Hisx (a, f x)
instance Functor f => Functor (Hisx f a) where
  fmap f (Hisx (a, x)) = Hisx (a, fmap f x)
-- | (Cofree f a) is Fixpoint for (Hisx f a)
newtype Cofree f a = Cf { unCf :: Fix (Hisx f a) }
instance Functor f => Functor (Cofree f) where
  fmap f = Cf . ana (phi . out) . unCf
    where
      phi (Hisx (a, x)) = Hisx (f a, x)
extract :: Functor f => Cofree f a -> a
extract cf = case out (unCf cf) of
  Hisx (a, _) -> a
sub :: Functor f => Cofree f a -> f (Cofree f a)
sub cf = case out (unCf cf) of
  Hisx (_, b) -> fmap Cf b

-- catamorphism
cata :: Functor f => (f a -> a) -> Fix f -> a
cata phi = phi . fmap (cata phi) . out
-- anamorphism
ana :: Functor f => (a -> f a) -> a -> Fix f
ana psi = In . fmap (ana psi) . psi
-- hylomorphism
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo phi psi = phi . fmap (hylo phi psi) . psi
-- histomorphism
histo :: Functor f => (f (HisF f t) -> t) -> Fix f -> t
histo phi = fst . unHis . cata (His . pair (phi, id))
-- dynamorphism
dyna :: Functor f => (f (Cofree f b) -> b) -> (a -> f a) -> a -> b
dyna phi psi = extract . hylo ap psi
  where
    ap a = Cf $ In $ Hisx (phi a, fmap unCf a)

foldn :: (Eq a, Num a) => (b, b -> b) -> a -> b
foldn (c, f) 0 = c
foldn (c, f) n = f (foldn (c, f) (n-1))

foldn2 :: (Eq a, Num a) => (t, t, t -> t -> t) -> a -> t
foldn2 (c, d, f) 0 = c
foldn2 (c, d, f) 1 = d
foldn2 (c, d, f) n = f (foldn2 (c, d, f) (n-2)) (foldn2 (c, d, f) (n-1))

paran2 :: (Eq a, Num a) => (t, t, a -> t -> t -> t) -> a -> t
paran2 (c, d, f) = u
  where
    u 0 = c
    u 1 = d
    u n = f n (u (n-2)) (u (n-1))

---------------------------------------------------------

main :: IO ()
main = do
  n <- readLn :: IO Int
  hs <- getIntVec n
  print $ fst $ step hs (n-1)

step :: U.Vector Int -> Int -> (Int, Int)
step vec = paran2 (c, d, f)
  where
    c = (0, vec U.! 0)
    d = (abs (h1-h), h) where (h1, h) = (vec U.! 0, vec U.! 1)
    f n (c2, h2) (c1, h1) = (min (c2+d2) (c1+d1), h)
      where
        h = vec U.! n
        (d2, d1) = (abs(h2-h), abs(h1-h))
