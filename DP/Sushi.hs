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
import qualified Data.Foldable as Foldable
import Data.List (unfoldr, foldl', sort, (\\), delete, nub)
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

getProblem :: IO (Int, U.Vector Int)
getProblem = do
  n <- readLn :: IO Int
  xs <-  getIntVec n
  return (n, xs)

data TreeF a x = Node a (Maybe x, Maybe x, Maybe x) deriving (Show, Functor)
type Tree a = Fix (TreeF a)
instance Show a => Show (Tree a) where
  show (In (Node a ns)) = "Node " ++ show a ++ " " ++ show ns

collect :: U.Vector Int -> (Int, Int, Int)
collect = U.foldl' f (0,0,0)
  where
    f (n1,n2,n3) x = case x of
      1 -> (n1+1, n2, n3)
      2 -> (n1, n2+1, n3)
      3 -> (n1, n2, n3+1)
      _ -> error "illegal number"

node :: a -> (Maybe (Tree a), Maybe (Tree a), Maybe (Tree a)) -> Tree a
node a xs = In (Node a xs)

node' :: ((Maybe a, Maybe a, Maybe a) -> a) -> b ->
         (Maybe (Cofree (TreeF b) a), Maybe (Cofree (TreeF b) a), Maybe (Cofree (TreeF b) a)) -> Cofree (TreeF b) a
node' f a (n1, n2, n3) = Cf (In (Hisx (f (fmap extract n1, fmap extract n2, fmap extract n3), Node a (fmap unCf n1, fmap unCf n2, fmap unCf n3))))

main :: IO ()
main = do
  (n, xys) <- getProblem
  print $ solve n (collect xys)

next :: Int -> (Int, Int, Int) -> (Int, Int, Int) -> [(Int, Int, Int)]
next n (x, y, z) (i, j, k)
  | i == x && j == y && k == z = []
  | otherwise = filter p [(i+1, j, k),(i-1, j+1, k),(i, j-1, k+1)]
  where
    p (i', j', k') = i' >= 0 && j' >= 0 && k' >= 0 && i'+j'+k' <= n && ijk' <= xyz && jk' <= yz && k' <= z
      where
        (jk', ijk') = (j'+k', i'+jk')
        (yz, xyz) = (y+z, x+yz)
    


solve :: Int -> (Int, Int, Int) -> Double
solve n ijk = extract (mkMap n ijk Map.! ijk)

mkMap :: Int -> (Int, Int, Int) -> Map.Map (Int, Int, Int) (Cofree (TreeF (Int, Int, Int)) Double)
mkMap n ijk = foldl' p (Map.singleton (0, 0, 0) nd000) (map snd $ Set.toList $ mkSet n ijk) -- (mkSeq n ijk)
  where
    conv i = maybe 0.0 (* fromIntegral i)
    f (i, j, k) (mt1, mt2, mt3) = (conv i mt1 + conv j mt2 + conv k mt3 + fromIntegral n) / fromIntegral (i + j + k)
    nd000 = node' (const 0.0) (0, 0, 0) (Nothing, Nothing, Nothing)
    nd ijk@(i, j, k) m = node' (f ijk) ijk (Map.lookup (i-1, j, k) m, Map.lookup (i+1, j-1, k) m, Map.lookup (i, j+1, k-1) m)
    p m key@(i, j, k) = Map.insert key (nd key m) m

mkSeq :: Int -> (Int, Int, Int) -> [(Int, Int, Int)]
mkSeq n xyz@(x, y, z) = concat $ unfoldr psi ini
  where
    ini = [(0, 0, 0)]
    psi xs | null xs' = Nothing
           | otherwise = Just (xs', xs')
      where
        xs' = nub $ concatMap next xs
    next (i, j, k)
      | i == x && j == y && k == z = []
      | otherwise = filter p [(i+1, j, k),(i-1, j+1, k),(i, j-1, k+1)]
      where
        p (i', j', k') = i' >= 0 && j' >= 0 && k' >= 0 && i'+j'+k' <= n && ijk' <= xyz && jk' <= yz && k' <= z
          where
            (jk', ijk') = (j'+k', i'+jk')
            (yz, xyz) = (y+z, x+yz)

-- for GHC 7.10
unions' :: (Foldable f, Ord a) => f (Set.Set a) -> Set.Set a
unions' = Foldable.foldl' Set.union Set.empty
{-# INLINABLE unions' #-}

mkSet :: Int -> (Int, Int, Int) -> Set.Set (Int, (Int, Int, Int))
mkSet n xyz@(x, y, z) = Set.unions $ unfoldr psi ini
  where
    ini :: Set.Set (Int, (Int, Int, Int))
    ini = Set.fromList [(0, (0, 0, 0))]
    psi :: Set.Set (Int, (Int, Int, Int)) -> Maybe (Set.Set (Int, (Int, Int, Int)), Set.Set (Int, (Int, Int, Int)))
    psi xs | Set.null xs' = Nothing
           | otherwise = Just (xs', xs')
      where
        xs' :: Set.Set (Int, (Int, Int, Int))
        xs' = unions' $ Set.map next xs
    next :: (Int, (Int, Int, Int)) -> Set.Set (Int, (Int, Int, Int))
    next (l, (i, j, k))
      | i == x && j == y && k == z = Set.fromList []
      | otherwise = Set.filter p $ Set.fromList [(l', (i+1, j, k)),(l', (i-1, j+1, k)),(l', (i, j-1, k+1))]
      where
        l' = l+1
        p (_, (i', j', k')) = i' >= 0 && j' >= 0 && k' >= 0 && i'+j'+k' <= n && ijk' <= xyz && jk' <= yz && k' <= z
          where
            (jk', ijk') = (j'+k', i'+jk')
            (yz, xyz) = (y+z, x+yz)
    
next' :: Int -> (Int, Int, Int) -> (Int, Int, Int) -> Set.Set (Int, Int, Int)
next' n (x, y, z) (i, j, k)
  | i == x && j == y && k == z = Set.empty
  | otherwise = Set.filter p $ Set.fromList [(i+1, j, k),(i-1, j+1, k),(i, j-1, k+1)]
  where
    p (i', j', k') = i' >= 0 && j' >= 0 && k' >= 0 && i'+j'+k' <= n && ijk' <= xyz && jk' <= yz && k' <= z
      where
        (jk', ijk') = (j'+k', i'+jk')
        (yz, xyz) = (y+z, x+yz)

{--
solve :: Int -> U.Vector Int -> Double
solve n xys = dyna phi psi (collect xys)
  where
    psi :: (Int, Int, Int) -> TreeF (Int, (Int, Int, Int)) (Int, Int, Int)
    psi ijk@(i, j, k) = Node (n, ijk) (next1 (i, j, k), next2 (i, j, k), next3 (i, j, k))
      where
        next1 (i, j, k) | i <= 0 = Nothing
                        | otherwise = Just (i-1, j, k)
        next2 (i, j, k) | j <= 0 = Nothing
                        | otherwise = Just (i+1, j-1, k)
        next3 (i, j, k) | k <= 0 = Nothing
                        | otherwise = Just (i, j+1, k-1)

    phi (Node _ (Nothing, Nothing, Nothing)) = 0.0
    phi (Node (n, (i,j,k)) (Just t1, Nothing, Nothing)) =
      (extract t1*fromIntegral i+fromIntegral n) / fromIntegral (i+j+k)
    phi (Node (n, (i,j,k)) (Nothing, Just t2, Nothing)) =
      (extract t2*fromIntegral j+fromIntegral n) / fromIntegral (i+j+k)
    phi (Node (n, (i,j,k)) (Nothing, Nothing, Just t3)) =
      (extract t3*fromIntegral k+fromIntegral n) / fromIntegral (i+j+k)
    phi (Node (n, (i,j,k)) (Nothing, Just t2, Just t3)) =
      (extract t2*fromIntegral j+extract t3*fromIntegral k+fromIntegral n) / fromIntegral (i+j+k)
    phi (Node (n, (i,j,k)) (Just t1, Nothing, Just t3)) =
      (extract t1*fromIntegral i+extract t3*fromIntegral k+fromIntegral n) / fromIntegral (i+j+k)
    phi (Node (n, (i,j,k)) (Just t1, Just t2, Nothing)) =
      (extract t1*fromIntegral i+extract t2*fromIntegral j+fromIntegral n) / fromIntegral (i+j+k)
    phi (Node (n, (i,j,k)) (Just t1, Just t2, Just t3)) =
      (extract t1*fromIntegral i+extract t2*fromIntegral j+extract t3*fromIntegral k+fromIntegral n) / fromIntegral (i+j+k)
--}

-- 1 1 1 => 3 0 0
example1 :: Double
example1 = extract nd300
  where
    conv i = maybe 0.0 (* fromIntegral i)
    f n (i, j, k) (mt1, mt2, mt3) = (conv i mt1 + conv j mt2 + conv k mt3 + fromIntegral n) / fromIntegral (i + j + k)
    nd300 = node' (f 3 (3, 0, 0)) (3, 0, 0) (Just nd200, Nothing, Nothing)
    nd200 = node' (f 3 (2, 0, 0)) (2, 0, 0) (Just nd100, Nothing, Nothing)
    nd100 = node' (f 3 (1, 0, 0)) (1, 0, 0) (Just nd000, Nothing, Nothing)
    nd000 = node' (const 0.0) (0, 0, 0) (Nothing, Nothing, Nothing)

-- 3 => 0 0 1
example2 :: Double
example2 = extract nd001
  where
    conv i = maybe 0.0 (* fromIntegral i)
    f n (i, j, k) (mt1, mt2, mt3) = (conv i mt1 + conv j mt2 + conv k mt3 + fromIntegral n) / fromIntegral (i + j + k)
    nd001 = node' (f 1 (0, 0, 1)) (0, 0, 1) (Nothing, Nothing, Just nd010)
    nd010 = node' (f 1 (0, 1, 0)) (0, 1, 0) (Nothing, Just nd100, Nothing)
    nd100 = node' (f 1 (1, 0, 0)) (1, 0, 0) (Just nd000, Nothing, Nothing)
    nd000 = node' (const 0.0) (0, 0, 0) (Nothing, Nothing, Nothing)

-- 1 2 => 1 1 0
example3 :: Double
example3 = extract nd110
  where
    conv i = maybe 0.0 (* fromIntegral i)
    f n (i, j, k) (mt1, mt2, mt3) = (conv i mt1 + conv j mt2 + conv k mt3 + fromIntegral n) / fromIntegral (i + j + k)
    nd110 = node' (f 2 (1, 1, 0)) (1, 1, 0) (Just nd010, Just nd200, Nothing)
    nd010 = node' (f 2 (0, 1, 0)) (0, 1, 0) (Nothing, Just nd100, Nothing)
    nd200 = node' (f 2 (2, 0, 0)) (2, 0, 0) (Just nd100, Nothing, Nothing)
    nd100 = node' (f 2 (1, 0, 0)) (1, 0, 0) (Just nd000, Nothing, Nothing)
    nd000 = node' (const 0.0) (0, 0, 0) (Nothing, Nothing, Nothing)

-- 1 1 1
sample111 :: Double
sample111 = extract nd111
  where
    conv i = maybe 0.0 (* fromIntegral i)
    f n (i, j, k) (mt1, mt2, mt3) = (conv i mt1 + conv j mt2 + conv k mt3 + fromIntegral n) / fromIntegral (i + j + k)
    nd111 = node' (f 3 (1, 1, 1)) (1, 1, 1) (Just nd011, Just nd201, Just nd120)
    nd011 = node' (f 3 (0, 1, 1)) (0, 1, 1) (Nothing, Just nd101, Just nd020)
    nd201 = node' (f 3 (2, 0, 1)) (2, 0, 1) (Just nd101, Nothing, Just nd210)
    nd120 = node' (f 3 (1, 2, 0)) (1, 2, 0) (Just nd020, Just nd210, Nothing)
    nd101 = node' (f 3 (1, 0, 1)) (1, 0, 1) (Just nd001, Nothing, Just nd110)
    nd020 = node' (f 3 (0, 2, 0)) (0, 2, 0) (Nothing, Just nd110, Nothing)
    nd210 = node' (f 3 (2, 1, 0)) (2, 1, 0) (Just nd110, Just nd300, Nothing)
    nd001 = node' (f 3 (0, 0, 1)) (0, 0, 1) (Nothing, Nothing, Just nd010)
    nd110 = node' (f 3 (1, 1, 0)) (1, 1, 0) (Just nd010, Just nd200, Nothing)
    nd300 = node' (f 3 (3, 0, 0)) (3, 0, 0) (Just nd200, Nothing, Nothing)
    nd010 = node' (f 3 (0, 1, 0)) (0, 1, 0) (Nothing, Just nd100, Nothing)
    nd200 = node' (f 3 (2, 0, 0)) (2, 0, 0) (Just nd100, Nothing, Nothing)
    nd100 = node' (f 3 (1, 0, 0)) (1, 0, 0) (Just nd000, Nothing, Nothing)
    nd000 = node' (const 0.0) (0, 0, 0) (Nothing, Nothing, Nothing)

-- 1 3 2 3 3 2 3 2 1 3 => 2 3 5
example4 :: Double
example4 = solve 10 (2, 3, 5)
