{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad (replicateM, forM_)
import Data.Bool (bool)
import Data.Char (isSpace)
import Data.Function (on)
import Data.Graph (Vertex, Edge, buildG, topSort)
import Data.List (unfoldr, foldl', sort, (\\), delete)
import qualified Data.IntMap as Map
import qualified Data.IntSet as Set
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as B
import System.IO (hPutStr, hPutStrLn, stdin, stdout, withFile, IOMode(..))

type Parser a = C.ByteString -> Maybe (a, C.ByteString)

parseInt :: Parser Int
parseInt = C.readInt . C.dropWhile isSpace

getInts :: IO [Int]
getInts = unfoldr parseInt <$> C.getLine

getIntVec :: Int -> IO (U.Vector Int)
getIntVec n = U.unfoldrN n parseInt <$> C.getLine

---------------------------------------------------------
{-# INLINE swap #-}
swap (x, y) = (y, x)
{-# INLINE pair #-}
pair (f, g) x = (f x, g x)
{-# INLINE cross #-}
cross (f, g) (x, y) = (f x, g y)

newtype Fix f = In { out :: f (Fix f) }

newtype Cofree f a = Cf { unCf :: (a, f (Cofree f a)) }
extract :: Cofree f t -> t
extract = fst . unCf
sub :: Functor f => Cofree f a -> f (Cofree f a)
sub = snd . unCf

newtype Free f a = Fr { unFr :: Either a (f (Free f a)) }
inject :: a -> Free f a
inject = Fr . Left

-- catamorphism
cata :: Functor f => (f a -> a) -> Fix f -> a
cata phi = phi . fmap (cata phi) . out
-- anamorphism
ana :: Functor f => (a -> f a) -> a -> Fix f
ana psi = In . fmap (ana psi) . psi
-- hylomorphism
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo phi psi = cata phi . ana psi -- phi . fmap (hylo phi psi) . psi
-- metamorphism
meta :: Functor f => (f a -> a) -> (a -> f a) -> Fix f -> Fix f
meta phi psi = ana psi . cata phi -- In . fmap (meta phi psi) . out
-- paramorphism
para :: Functor f => (f (Fix f, t) -> t) -> Fix f -> t
para phi = phi . fmap (pair (id, para phi)) . out
-- apomorphism
apo :: Functor f => (t -> f (Either (Fix f) t)) -> t -> Fix f
apo psi = In . fmap (uncurry either (id, apo psi)) . psi
-- histomorphism
histo :: Functor f => (f (Cofree f t) -> t) -> Fix f -> t
histo phi = extract . cata (Cf . pair (phi, id))
-- futumorphism
futu :: Functor f => (t -> f (Free f t)) -> t -> Fix f
futu psi = ana (uncurry either (psi, id) . unFr) . inject
-- chronomorphism
chrono :: Functor f => (f (Cofree f b) -> b) -> (a -> f (Free f a)) -> a -> b
chrono phi psi = histo phi . futu psi
-- cochronomorphism
cochrono :: Functor f => (f (Cofree f t) -> t) -> (t -> f (Free f t)) -> Fix f -> Fix f
cochrono phi psi = futu psi . histo phi
-- zygomorphism
zygo :: Functor f => (f a -> a) -> (f (a, b) -> b) -> Fix f -> b
zygo f phi = snd . cata (pair (f . fmap fst, phi))
-- cozygomorphism
cozygo :: Functor f => (a -> f a) -> (b -> f (Either a b)) -> b -> Fix f
cozygo f psi = ana (uncurry either (fmap Left . f, psi)) . Right
-- dynamorphism
dyna :: Functor f => (f (Cofree f b) -> b) -> (a -> f a) -> a -> b
dyna f g = chrono f (fmap inject . g) -- histo f . ana g
-- codynamorphism
codyna :: Functor f => (f b -> b) -> (a -> f (Free f a)) -> a -> b
codyna f g = chrono (f . fmap extract) g
-- mutumorphism
mutu :: Functor f => (a -> b) -> (f a -> a) -> Fix f -> b
mutu proj phi = proj . cata phi
-- comutumorphism
comutu :: Functor f => (b -> a) -> (a -> f a) -> b -> Fix f
comutu proj psi = ana psi . proj


foldn (c, f) 0 = c
foldn (c, f) n = f (foldn (c, f) (n-1))

paran (c, f) 0 = c
paran (c, f) n = f n (paran (c, f) (n-1))

---------------------------------------------------------

getTuple :: IO Edge
getTuple = do
  (x:y:_) <- getInts
  return (x, y)

getIdxTuple :: IO Edge
getIdxTuple = cross (pred, pred) <$> getTuple

getProblem :: IO (Int, Int, U.Vector Edge)
getProblem = do
  (n, m) <- getTuple
  xys <- U.replicateM m getIdxTuple
  return (n, m, xys)

data NonEmptyListF a = NonEmptyListF (Vertex, [Vertex], [Int]) (Maybe a) deriving (Show, Functor)

targets :: Int -> U.Vector Edge -> V.Vector [Vertex]
targets n es = V.create $ do
  vec <- VM.replicate n []
  U.forM_ es $ \(s, t) -> do
    VM.modify vec (t:) s
  return vec

transposition :: Map.IntMap Int -> Map.Key -> Map.Key -> Int
transposition dict = flip ((-) `on` (dict Map.!))

transpositions :: Map.IntMap Int -> Map.Key -> [Map.Key] -> [Int]
transpositions dict = map . (transposition dict)

compileWith :: Map.IntMap Int -> V.Vector [Map.Key] -> U.Vector Map.Key -> V.Vector [Int]
compileWith dict ds vs = V.create $ do
  vec <- VM.replicate (U.length vs) []
  U.forM_ vs $ \s -> do
    VM.write vec (dict Map.! s) $ transpositions dict s (ds V.! s)
  return vec

main :: IO ()
main = do
  (n, m, xys) <- getProblem
  print $ solve n xys

gensample = withFile "DP/lp-sample.txt" WriteMode $ \h -> do
  let xs = [(i,i+j) | i <- [1..1000], j <- [1..1000]]
  let (x, y) = (max 10000 (max (maximum (map fst xs)) (maximum (map snd xs))), length xs)
  hPutStr h (show x)
  hPutStr h " "
  hPutStrLn h (show y)
  forM_ xs $ \(x, y) -> do
    hPutStr h (show x)
    hPutStr h " "
    hPutStrLn h (show y)

solve :: Int -> U.Vector Edge -> Int
solve n xys = dyna phi psi (n-1)
  where
    !n' = n-1
    vs :: U.Vector Vertex
    !vs = U.fromList . topSort . buildG (0, n') . U.toList $ xys
    ds :: V.Vector [Vertex]
    ds = targets n xys
    dict :: Map.IntMap Int
    dict = Map.fromList . U.toList . U.map swap . U.indexed $ vs
    ds' :: V.Vector [Int]
    ds' = compileWith dict ds vs
    !nosupports = Set.toList $ Set.fromAscList [0..n'] Set.\\ (Set.fromList $ map snd $ U.toList xys)
    !startlist = sort $ delete 0 $ transpositions dict (vs U.! 0) nosupports

    psi 0 = NonEmptyListF (vs U.! n', sort $ ds' V.! n', []) Nothing
    psi i = NonEmptyListF (vs U.! (n'-i), sort $ ds' V.! (n'-i), bool [] startlist (i == n')) (Just (i-1))

    phi :: NonEmptyListF (Cofree NonEmptyListF Int) -> Int
    phi (NonEmptyListF _ Nothing) = 0
    phi prev@(NonEmptyListF (_, bps, ss) (Just t))
      | null bps = back 0 1 ss prev
      | otherwise = max (back 0 1 bps prev + 1) (back 0 1 ss prev)


    back :: Int -> Int -> [Int] -> NonEmptyListF (Cofree NonEmptyListF Int) -> Int
    back ret i [] _ = ret
    back ret i bps@(j:js) nel@(NonEmptyListF _ mv)
      | i == j = maybe ret (\t -> back (max ret (extract t)) (i+1) js (sub t)) mv
      | otherwise = let Just t = mv in back ret (i+1) bps (sub t)
