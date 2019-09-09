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
import qualified Data.Vector.Unboxed as U
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

main :: IO ()
main = do
  Just ((!n, !q), _) <- parseInt2 <$> C.getLine
  !aq <- U.unfoldrN (n-1+q) parseInt2 <$> C.getContents
  let (!a, !q) = U.splitAt (n-1) aq
  let !g = genTree n a
  let !ans = U.map (\i -> U.foldr (\(i', pt) ttl -> if path g (i'-1) i then pt+ttl else ttl) 0 q) $ U.enumFromN 0 n
  putStrLn $ intercalate " " (map show (U.toList ans))

genTree :: Int -> U.Vector (Int, Int) -> Graph
genTree n ps = let (!g, _, _) = graphFromEdges (V.toList ns) in g
  where
    cs :: V.Vector (U.Vector Int)
    !cs = V.generate n $ \i -> U.map snd (U.filter ((==(i+1)).fst) ps)
    ns :: V.Vector (Int, Int, [Int])
    !ns = V.zipWith (\xs i -> (i, i+1, U.toList xs)) cs (V.enumFromN 0 n)

ex1 :: (U.Vector (Int, Int), U.Vector (Int, Int))
ex1 = (U.fromList [(1,2),(2,3),(2,4)], U.fromList [(2,10),(1,100),(3,1)])
