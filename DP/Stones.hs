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
import Data.List (unfoldr, foldl', sort, sortBy, (\\), delete, nub)
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

data NonEmptyListF a = NonEmptyListF (Int, [Int]) (Maybe a) deriving (Show, Functor)

type NonEmptyList = Fix NonEmptyListF
instance Show NonEmptyList where
  show (In (NonEmptyListF h Nothing)) = show h ++ ":[]"
  show (In (NonEmptyListF h (Just t))) = show h ++ ":" ++ show t

data Player = First | Second deriving (Show, Eq)

main = do
  (n:k:_) <- getInts
  xs <- getInts
  print $ bool Second First $ solve (sort xs) k

solve :: [Int] -> Int -> Bool
solve !xs = dyna phi psi
  where
    psi 0 = NonEmptyListF (0, []) Nothing
    psi i = NonEmptyListF (i, (takeWhile (>=0) . map (i-)) xs) (Just (i-1))

    phi :: NonEmptyListF (Cofree NonEmptyListF Bool) -> Bool
    phi (NonEmptyListF _ Nothing) = False
    phi prev@(NonEmptyListF (i, !bs) (Just !t))
      | null bs = False
      | otherwise = back bs prev

    back :: [Int] -> NonEmptyListF (Cofree NonEmptyListF Bool) -> Bool
    back _ (NonEmptyListF _ Nothing) = False
    back [] (NonEmptyListF _ (Just !t)) = False
    back kks@(k:ks) (NonEmptyListF (j, _) (Just t))
      | k+1 == j = extract t == False || (back ks $! sub t)
      | otherwise = back kks $! sub t
