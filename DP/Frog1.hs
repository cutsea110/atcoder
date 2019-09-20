{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
module Main where

import Data.Char (isSpace)
import qualified Data.Vector.Unboxed as U
import qualified Data.ByteString.Char8 as C

type Parser a = C.ByteString -> Maybe (a, C.ByteString)

parseInt :: Parser Int
parseInt = C.readInt . C.dropWhile isSpace

getIntVec :: Int -> IO (U.Vector Int)
getIntVec n = U.unfoldrN n parseInt <$> C.getLine

----------------------------
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
dyna f g = chrono f (fmap inject . g)
-- codynamorphism
codyna :: Functor f => (f b -> b) -> (a -> f (Free f a)) -> a -> b
codyna f g = chrono (f . fmap extract) g
-- mutumorphism
mutu :: Functor f => (a -> b) -> (f a -> a) -> Fix f -> b
mutu proj phi = proj . cata phi


foldn (c, f) 0 = c
foldn (c, f) n = f (foldn (c, f) (n-1))

paran (c, f) 0 = c
paran (c, f) n = f n (paran (c, f) (n-1))

---------------------------------------------------------

frog1 vec = paran (step vec 0 undefined, step vec)

step vec 0 _ = (0, (vec U.! 0, undefined, undefined, undefined, undefined))
step vec 1 (c, (h, h1, c1, h2, c2)) = (c', (h', h, c, h1, c1))
  where
    h' = vec U.! 1
    c' = abs (h'-h)
step vec i (c, (h, h1, c1, h2, c2)) = (c', (h', h1', c1', h2', c2'))
  where
    h' = vec U.! i
    h1' = h
    c1' = c
    h2' = h1
    c2' = c1
    d1 = abs (h'-h1')
    d2 = abs (h'-h2')
    c' = min (c1'+d1) (c2'+d2)

main :: IO ()
main = do
  !n <- readLn :: IO Int
  !hs <- getIntVec n
  print $ fst $ frog1 hs (n-1)

{-
data TNat a = Zero | Succ a deriving (Show, Functor)
type Nat = Fix TNat

nat 0 = In Zero
nat n = In $ Succ $ nat (n-1)

fibAsHisto :: Nat -> Integer
fibAsHisto = histo phi where
  phi Zero = 0 -- (a)
  phi (Succ x) = f1 + f2 where -- (b)
    f1 = extract x
    f2 = case sub x of
      Zero   -> 1 -- (c1)
      Succ y -> extract y -- (c2)
-}
