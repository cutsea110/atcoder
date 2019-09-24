{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Monad
import Data.Bool (bool)
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as UM
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Unsafe as B

type Parser a = C.ByteString -> Maybe (a, C.ByteString)

parseInt :: Parser Int
parseInt = C.readInt . C.dropWhile isSpace

getInts :: IO [Int]
getInts = unfoldr parseInt <$> C.getLine

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

main :: IO ()
main = do
  !s <- C.cons '\NUL' <$> C.getLine
  !t <- C.cons '\NUL' <$> C.getLine
  let solved = solve s t
      (n,_,_,ss) = head $ regain' solved t
  --forM_ ss putStrLn
  if n == 0 then putChar '\n'
    else putStrLn . head $ ss

data NonEmptyListF a b = NonEmptyListF a (Maybe b) deriving (Show, Functor)

regain :: V.Vector (U.Vector (Char, Int)) -> C.ByteString -> String
regain solved rs = go [] (rlen-1, clen-1)
  where
    (rlen, clen) = (V.length solved, U.length $ V.head solved)
    !mat = V.reverse solved
    
    go res (i, j) | i <= 0 || j <= 0 = res
                  | n == l = go res (i, j-1)
                  | n == u = go res (i-1, j)
                  | otherwise = go (c':res) (i-1, j-1)
      where
        c = rs `C.index` i
        (c', n) = (mat V.! i) U.! j
        (_, l) = (mat V.! i) U.! (j-1)
        (_, u) = (mat V.! (i-1)) U.! j

regain' :: V.Vector (U.Vector (Char, Int)) -> C.ByteString -> [(Int, (Int, Int), Char, [String])]
regain' solved rs = map (n4th (map reverse)) $ dyna phi psi (maxlen, candidates)
  where
    !rlen = V.length solved - 1
    rs' = V.fromList . C.unpack . C.reverse $ rs
    maxlen = snd . U.last . V.head $ solved

    n4th f (x,y,z,w) = (x,y,z,f w)
    
    !candidates = V.ifoldr pi U.empty $ V.zip rs' solved
      where
        pi !i (!c, !v) = (U.ifoldr pj U.empty v U.++)
          where
            pj !j (!c', !n) !res = if c /= c' then res
                                   else (n,(rlen-i,j),c') `U.cons` res

    psi (0, cs) = NonEmptyListF cs Nothing
    psi (i, cs) = NonEmptyListF xs (Just (i-1, ys))
      where
        (xs, ys) = U.partition (\(n,_,_) -> n == i) cs

    phi (NonEmptyListF v Nothing) = map (\(n,(i,j),c) -> (n,(i,j),c,[""])) $ U.toList v
    phi (NonEmptyListF v (Just t)) = ret
      where
        prev = extract t
        !ret = map (\(n,(i,j),c) ->
                      let tmp = filter (\(_,(i',j'),_,_) -> i'<i && j'<j) prev
                          res = concatMap (\(_,_,_,r) -> map (c:) r) tmp
                      in (n,(i,j),c,res)) $ U.toList v

solve :: C.ByteString -> C.ByteString -> V.Vector (U.Vector (Char, Int))
solve !cs !rs = dyna phi psi (rlen-1)
  where
    (!clen, !rlen) = (C.length cs, C.length rs)
    
    psi 0 = NonEmptyListF (rs `C.index` 0) Nothing
    psi i = NonEmptyListF (rs `C.index` i) (Just (i-1))

    phi :: NonEmptyListF Char (Cofree (NonEmptyListF Char) (V.Vector (U.Vector (Char, Int)))) -> V.Vector (U.Vector (Char, Int))
    phi (NonEmptyListF _ Nothing) = V.singleton $ U.generate clen $ \i -> (cs `C.index` i, 0)
    phi (NonEmptyListF !c (Just t)) = vec `V.cons` prevs
      where
        prevs = extract t
        prev = V.head prevs
        vec = U.cons ('\NUL', 0) $! U.unfoldr p (0, U.zip prev (U.tail prev))
          where
            p (!l, !xs) | U.null xs = Nothing
                        | otherwise =
                            let (((_, !lu), (!c', !u)), !xs') = (U.head xs, U.tail xs)
                                !l' = bool (max l u) (lu+1) (c==c')
                            in Just ((c', l'), (l', xs'))
