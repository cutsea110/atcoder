{-# LANGUAGE BangPatterns
           , LambdaCase
#-}
module Main where

import GHC.Real (toInteger)
import Control.Arrow (first)
import Control.Monad (replicateM, forM_)
import Control.Monad.ST (runST, ST)
import Data.Array.ST (newListArray, getElems, readArray, writeArray, STArray)
import Data.Char (isSpace)
import Data.List (unfoldr, intercalate, sort)
import qualified Data.ByteString.Char8 as C
import qualified Data.Vector.Unboxed as U

type Parser a = C.ByteString -> Maybe (a, C.ByteString)

parseInt :: Parser Int
parseInt = C.readInt . C.dropWhile isSpace

parseInteger :: Parser Integer
parseInteger = fmap (first toInteger) . parseInt

getInts :: IO [Int]
getInts = unfoldr parseInt <$> C.getLine

getIntegers :: IO [Integer]
getIntegers = unfoldr parseInteger <$> C.getLine

getIntVec :: Int -> IO (U.Vector Int)
getIntVec n = U.unfoldrN n parseInt <$> C.getLine

getTuple :: MonadFail m => m [a] -> m (a, a)
getTuple p = do
  (x:y:_) <- p
  return (x, y)

getTuples :: (Monad m, Integral a) => m (a, a) -> a -> m [(a, a)]
getTuples p n = replicateM (fromIntegral n) p

--------------------------------------------------------------------------------
-- insertion sort (known length)
insertionSort :: Ord a => Int -> [a] -> [a]
insertionSort n ks = runST $ do
  a <- newListArray (1, n) ks
  isort n a
  getElems a

isort :: Ord a => Int -> STArray s Int a -> ST s ()
isort n ks = forM_ [2..n] $ \j -> do
  kj <- readArray ks j
  insert ks (j-1) kj

insert :: Ord a => STArray s Int a -> Int -> a -> ST s ()
insert ks i k = do
  ki <- readArray ks i
  if k >= ki
  then writeArray ks (i+1) k
  else do writeArray ks (i+1) ki
          if i-1 > 0
            then insert ks (i-1) k
            else writeArray ks i k

-- bubble sort (known length)
bubbleSort :: Ord a => Int -> [a] -> [a]
bubbleSort n ks = runST $ do
  a <- newListArray (1, n) ks
  bsort n a
  getElems a

bsort :: Ord a => Int -> STArray s Int a -> ST s ()
bsort bound ks = swap ks 0 [1..bound-1] >>= \t ->
  if t == 0
  then return ()
  else bsort t ks

swap :: Ord a => STArray s Int a -> Int -> [Int] -> ST s Int
swap ks t = \case
  [] -> return t
  (j:js) -> do
    kj <- readArray ks j
    kj1 <- readArray ks (j+1)
    if kj > kj1
      then do writeArray ks j kj1
              writeArray ks (j+1) kj
              swap ks j js
      else swap ks t js

-- selection sort (known length)
selectionSort :: Ord a => Int -> [a] -> [a]
selectionSort n ks = runST $ do
  a <- newListArray (1, n) ks
  ssort n a
  getElems a

ssort :: Ord a => Int -> STArray s Int a -> ST s ()
ssort n ks = forM_ [n, n-1 .. 2] $ \j -> do
  i <- findMax ks j (j-1)
  ki <- readArray ks i
  kj <- readArray ks j
  writeArray ks i kj
  writeArray ks j ki

findMax :: Ord a => STArray s Int a -> Int -> Int -> ST s Int
findMax ks i k = do
  ki <- readArray ks i
  kk <- readArray ks k
  let i' = if ki >= kk then i else k
  if k > 1
    then findMax ks i' (k-1)
    else return i'

--------------------------------------------------------------------------------

main = do
  n  <- head <$> getInts
  hs <- getInts
  putStrLn . intercalate " " . map show . mergeSort $ hs

mergeSort :: [Int] -> [Int]
mergeSort = undefined
