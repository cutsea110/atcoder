{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Arrow ((***), (&&&), first, second)
import Control.Monad (replicateM, forM_)
import Data.Array ((!))
import Data.Bool (bool)
import Data.Char (isSpace)
import Data.Function (on)
import Data.Graph (Graph, Vertex, Edge, buildG, topSort)
import Data.List (unfoldr, foldl', sort, (\\), delete)
import qualified Data.Map as Map
import qualified Data.Set as Set
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
swap (x, y) = (y, x)
pair (f, g) x = (f x, g x)
cross (f, g) (x, y) = (f x, g y)

newtype Fix f = In { out :: f (Fix f) }

data Hisx f a x = Hisx { unHisx :: (a, f x) } deriving (Show, Functor)
newtype Cofree f a = Cf { unCf :: Fix (Hisx f a) }
instance Functor f => Functor (Cofree f) where
  fmap f = Cf . ana (phi . out) . unCf
    where phi (Hisx (a, x)) = Hisx (f a, x)

extract :: Functor f => Cofree f t -> t
extract cf = case out (unCf cf) of
  Hisx (a, _) -> a

sub :: Functor f => Cofree f a -> f (Cofree f a)
sub cf = case out (unCf cf) of
  Hisx (_, b) -> fmap Cf b

data Futx f a x = Futx { unFutx :: Either a (f x) } deriving (Show, Functor)
newtype Free f a = Fr { unFr :: Fix (Futx f a) }
instance Functor f => Functor (Free f) where
  fmap f = Fr . cata (In . phi) . unFr
    where phi (Futx (Left a)) = Futx (Left (f a))
          phi (Futx (Right x)) = Futx (Right x)

inject :: a -> Free f a
inject = Fr . In . Futx . Left

-- catamorphism
cata :: Functor f => (f a -> a) -> Fix f -> a
cata phi = phi . fmap (cata phi) . out
-- anamorphism
ana :: Functor f => (a -> f a) -> a -> Fix f
ana psi = In . fmap (ana psi) . psi
-- hylomorphism
hylo :: Functor f => (f b -> b) -> (a -> f a) -> a -> b
hylo phi psi = phi . fmap (hylo phi psi) . psi -- cata phi . ana psi
-- metamorphism
meta :: Functor f => (f a -> a) -> (a -> f a) -> Fix f -> Fix f
meta phi psi = In . fmap (meta phi psi) . out -- ana psi . cata phi
-- paramorphism
para :: Functor f => (f (Fix f, t) -> t) -> Fix f -> t
para phi = phi . fmap (pair (id, para phi)) . out
-- apomorphism
apo :: Functor f => (t -> f (Either (Fix f) t)) -> t -> Fix f
apo psi = In . fmap (uncurry either (id, apo psi)) . psi
-- histomorphism
histo :: Functor f => (f (Cofree f t) -> t) -> Fix f -> t
histo phi = extract . cata ap
  where ap = cast . Hisx . pair (phi, id)
        cast = Cf . In . fmap unCf
-- futumorphism
futu :: Functor f => (t -> f (Free f t)) -> t -> Fix f
futu psi = ana ap . inject
  where ap = uncurry either (psi, id) . unFutx . cast
        cast = fmap Fr . out . unFr
-- chronomorphism
chrono :: Functor f => (f (Cofree f b) -> b) -> (a -> f (Free f a)) -> a -> b
chrono phi psi = extract . hylo phi' psi' . inject
  where
    phi' = toCofree . Hisx . pair (phi, id)
    toCofree = Cf . In . fmap unCf
    psi' = uncurry either (psi, id) . unFutx . fromFree
    fromFree = fmap Fr . out . unFr
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

getProblem :: IO (Int, Int, U.Vector Edge)
getProblem = do
  (n, m) <- getTuple
  xys <- U.replicateM m getTuple
  return (n, m, xys)

data TreeF a x = Tip a | Node a [x] deriving (Show, Functor)
type Tree a = Fix (TreeF a)
instance Show a => Show (Tree a) where
  show (In (Tip x)) = "Tip " ++ show x
  show (In (Node a xs)) = "Node " ++ show a ++ " " ++ show xs

tip :: a -> Tree a
tip a = In (Tip a)
node :: a -> [Tree a] -> Tree a
node a xs = In (Node a xs)

tip' :: b -> a -> Cofree (TreeF a) b
tip' c n = Cf (In (Hisx (c, Tip n)))
node' :: ([a] -> a) -> b -> [Cofree (TreeF b) a] -> Cofree (TreeF b) a
node' f a xs = Cf (In (Hisx (f (map extract xs), Node a (map unCf xs))))

main :: IO ()
main = do
  (n, m, xys) <- getProblem
  print . fst . extract $ solve n xys

solve :: Int -> U.Vector Edge -> Cofree (TreeF Vertex) (Int, [Vertex])
solve n xys = node' (second reverse.maximum) 0 (Map.elems m)
  where
    mkT :: Vertex -> Cofree (TreeF Vertex) (Int, [Vertex])
    mkT i = tip' (0,[i]) i
    mkN :: Vertex -> [Cofree (TreeF Vertex) (Int, [Vertex])] -> Cofree (TreeF Vertex) (Int, [Vertex])
    mkN i = node' (((+1) *** (i:)).maximum) i

    g = buildG (1,n) $ U.toList xys
    sorted = topSort g
    m = foldr entry Map.empty sorted
    entry i m | null xs = Map.insert i (mkT i) m
              | otherwise = Map.insert i (mkN i (map (m Map.!) xs)) m
      where xs = g ! i

-- [FIXME] this break nexas construction!
solve' :: Int -> U.Vector Edge -> (Int, [Vertex])
solve' n xys = dyna phi psi (0, starts)
  where
    starts :: [Vertex]
    starts = let (ss, es) = unzip (U.toList xys) in Set.toList $ (Set.fromList ss) Set.\\ (Set.fromList es)
    g :: Graph
    g = buildG (1,n) $ U.toList xys
    m :: Map.Map Vertex [Vertex]
    m = Map.fromList $ map (id &&& (g !)) [1..n]

    psi :: (Vertex, [Vertex]) -> TreeF Vertex (Vertex, [Vertex])
    psi (n, []) = Tip n
    psi (n, xs) = Node n (map (id &&& (m Map.!)) xs)
    
    phi :: TreeF Vertex (Cofree (TreeF Vertex) (Int, [Vertex])) -> (Int, [Vertex])
    phi (Tip n) = (0, [n])
    phi (Node n ts) = ((+1) *** (n:)) $ maximum $ map extract ts

{-
-- ex1 :: (Int, [Int])
ex1 = [nd1, nd2, nd3, nd4] -- extract top
  where
    top :: Cofree (TreeF Vertex) (Int, [Vertex])
    top = node' (second reverse.maximum) 0 [nd1,nd2,nd3,nd4]
    mkT :: Vertex -> Cofree (TreeF Vertex) (Int, [Vertex])
    mkT i = tip' (0,[i]) i
    mkN :: Vertex -> [Cofree (TreeF Vertex) (Int, [Vertex])] -> Cofree (TreeF Vertex) (Int, [Vertex])
    mkN i = node' (((+1) *** (i:)).maximum) i
    nd1 = mkN 1 [nd3,nd2]
    nd3 = mkN 3 [nd4,nd2]
    nd2 = mkN 2 [nd4]
    nd4 = mkT 4

ex2 :: (Int, [Int])
ex2 = extract top
  where
    top = node' (second reverse.maximum) 0 [nd1,nd2,nd3,nd4,nd5,nd6]
    mkT :: Vertex -> Cofree (TreeF Vertex) (Int, [Vertex])
    mkT i = tip' (0,[i]) i
    mkN :: Vertex -> [Cofree (TreeF Vertex) (Int, [Vertex])] -> Cofree (TreeF Vertex) (Int, [Vertex])
    mkN i = node' (((+1) *** (i:)).maximum) i
    nd4 = mkN 4 [nd5]
    nd5 = mkN 5 [nd6]
    nd6 = mkT 6
    nd2 = mkN 2 [nd3]
    nd3 = mkT 3
    nd1 = mkT 1

ex3 :: (Int, [Int])
ex3 = extract top
  where
    top = node' (second reverse.maximum) 0 [nd1,nd2,nd3,nd4,nd5]
    mkT :: Vertex -> Cofree (TreeF Vertex) (Int, [Vertex])
    mkT i = tip' (0,[i]) i
    mkN :: Vertex -> [Cofree (TreeF Vertex) (Int, [Vertex])] -> Cofree (TreeF Vertex) (Int, [Vertex])
    mkN i = node' (((+1) *** (i:)).maximum) i
    nd5 = mkN 5 [nd1,nd2,nd3]
    nd2 = mkN 2 [nd4,nd3]
    nd1 = mkN 1 [nd3,nd4]
    nd4 = mkN 4 [nd3]
    nd3 = mkT 3
-}
