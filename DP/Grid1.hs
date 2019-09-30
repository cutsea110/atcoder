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
histo phi = extract . cata (Cf . pair (phi, id))
-- futumorphism
futu :: Functor f => (t -> f (Free f t)) -> t -> Fix f
futu psi = ana (uncurry either (psi, id) . unFr) . inject
-- chronomorphism
chrono :: Functor f => (f (Cofree f b) -> b) -> (a -> f (Free f a)) -> a -> b
chrono phi psi = extract . hylo phi' psi' . inject
  where
    phi' = Cf . pair (phi, id)
    psi' = uncurry either (psi, id) . unFr
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

data NonEmptyListF a = NonEmptyListF C.ByteString (Maybe a) deriving (Show, Functor)

conv :: C.ByteString -> U.Vector Char
conv bs = U.generate (C.length bs) (bs `C.index`)

main :: IO ()
main = do
  (h, w) <- getTuple
  bss <- V.replicateM h C.getLine
  let n = solve (h, w) bss
  print $ U.last n `mod` (10^9+7)

line0 :: C.ByteString -> U.Vector Int
line0 = U.fromList . scanl1 (*) . C.foldr (\c s -> if c == '.' then 1:s else 0:s) []

lineI :: U.Vector Int -> C.ByteString -> U.Vector Int
lineI vs = U.tail . U.scanl' f 0 . U.zip vs . conv
  where
    f _ (_, '#') = 0
    f l (u, '.') = u+l

solve :: (Int, Int) -> V.Vector C.ByteString -> U.Vector Int
solve (h, w) bss = dyna phi psi (h-1)
  where
    psi 0 = NonEmptyListF (bss V.! 0) Nothing
    psi i = NonEmptyListF (bss V.! i) (Just (i-1))

    phi (NonEmptyListF bs Nothing) = line0 bs
    phi (NonEmptyListF bs (Just t)) = lineI (extract t) bs
