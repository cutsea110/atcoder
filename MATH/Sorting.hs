{-# LANGUAGE BangPatterns
           , LambdaCase
#-}
module Main where

import GHC.Real (toInteger)
import Control.Arrow (first)
import Control.Monad (replicateM, forM_, when, unless)
import Control.Monad.ST (runST, ST)
import Data.Array.ST (newListArray, getElems, readArray, writeArray
                     , STArray, MArray (newArray_, newArray), Ix)
import Data.Char (isSpace)
import Data.Bool (bool)
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
-- ref.) https://zenn.dev/yoshikuni_jujo/articles/sort-algorithms
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

-- quick sort (known length)
quickSort :: (Ord a, Bounded a) => Int -> [a] -> [a]
quickSort = quickSortM 32

quickSortM :: (Ord a, Bounded a) => Int -> Int -> [a] -> [a]
quickSortM m n ks = init . tail $ runST $ do
  a <- array n ks
  qsort m n a
  getElems a

array :: Bounded a => Int -> [a] -> ST s (STArray s Int a)
array n ks = do
  a <- newArray_ (0, n + 1)
  writeArray a 0 minBound
  writeArray a (n + 1) maxBound
  mapM_ (uncurry (writeArray a)) $ zip [1..] ks
  return a

qsort :: Ord a => Int -> Int -> STArray s Int a -> ST s ()
qsort m n ks = do
  when (n>m) $ stage m ks 1 n
  when (m>1) $ isort n ks

stage :: Ord a => Int -> STArray s Int a -> Int -> Int -> ST s ()
stage m ks l r = do
  k <- readArray ks l
  (j, kj) <- exchange ks k (l + 1) r
  writeArray ks l kj
  writeArray ks j k
  case (r - j >= j - l, r - j > m, j - l > m) of
    (True,  _,     True ) -> do
      stage m ks l (j-1)
      stage m ks (j+1) r
    (False, True,  _    ) -> do
      stage m ks (j+1) r
      stage m ks l (j-1)
    (_,     True,  False) -> stage m ks (j+1) r
    (_,     False, True ) -> stage m ks l (j-1)
    _ -> return ()

exchange :: Ord a => STArray s Int a -> a -> Int -> Int -> ST s (Int, a)
exchange ks k i j = do
  (i', ki) <- fromL i (<k) ks
  (j', kj) <- fromR j (k<) ks
  if j' <= i'
    then return (j', kj)
    else do
    writeArray ks i' kj
    writeArray ks j' ki
    exchange ks k (i'+1) (j'-1)

fromL :: Int -> (a -> Bool) -> STArray s Int a -> ST s (Int, a)
fromL i p ks = do
  k <- readArray ks i
  if p k
    then fromL (i + 1) p ks
    else return (i, k)

fromR :: Int -> (a -> Bool) -> STArray s Int a -> ST s (Int, a)
fromR j p ks = do
  k <- readArray ks j
  if p k
    then fromR (j - 1) p ks
    else return (j, k)

-- heap sort (known length)
heapSort :: Ord a => Int -> [a] -> [a]
heapSort n ks = runST $ do
  a <- newListArray (1, n) ks
  hsort m n a
  getElems a
  where m = n `div` 2

hsort :: Ord a => Int -> Int -> STArray s Int a -> ST s ()
hsort m n ks = do
  forM_ [m, m - 1 .. 1] $ \l -> do
    a <- readArray ks l
    shiftup ks l n a
  forM_ [n, n - 1 .. 2] $ \r -> do
    k <- readArray ks r
    h <- readArray ks 1
    writeArray ks r h
    shiftup ks 1 (r - 1) k

shiftup :: Ord a => STArray s Int a -> Int -> Int -> a -> ST s ()
shiftup ks j r k
  | c <= r = do
      kc <- readArray ks c
      (e, ke) <- if c < r
                 then do kd <- readArray ks d
                         if kc < kd
                           then return (d, kd)
                           else return (c, kc)
                 else return (c, kc)
      if k >= ke
        then writeArray ks j k
        else do
        writeArray ks j ke
        shiftup ks e r k
  | otherwise = writeArray ks j k
  where c = 2 * j
        d = c + 1

-- merge sort (known length)
{-# INLINE copy #-}
copy :: (MArray a e m, Ix i) => a i e -> i -> i -> m ()
copy a d s = do
  k <- readArray a s
  writeArray a d k

mergeSort :: Ord a => Int -> [a] -> [a]
mergeSort n ks = take n $ runST $ do
  a <- prepareArray n ks
  nsort n False a
  getElems a

prepareArray :: Int -> [a] -> ST s (STArray s Int a)
prepareArray n ks = do
  a <- newArray_ (1, n * 2)
  mapM_ (uncurry (writeArray a)) $ zip [1 ..] ks
  return a

nsort :: Ord a => Int -> Bool -> STArray s Int a -> ST s ()
nsort n s ks = do
  b <- inner ks i j k l 1 True
  if b
    then unless s $ forM_ [1 .. n] $ \m -> copy ks m (n + m)
    else nsort n (not s) ks
  where (i,j,k,l)
          | s         = (n+1, 2*n,   1,   n)
          | otherwise = (  1,   n, n+1, 2*n)
  
inner :: Ord a => STArray s Int a -> Int -> Int -> Int -> Int -> Int -> Bool -> ST s Bool
inner ks i j k l d f
  | i == j    = do
      copy ks k i
      return f
  | otherwise = do
      ki <- readArray ks i
      kj <- readArray ks j
      if ki > kj
      then transR ks i j k d >>= \case
        Nothing       -> inner ks i  j' k' l  d     f
        Just (ii, kk) -> inner ks ii j' l  kk (- d) False
      else transL ks i j k d >>= \case
        Nothing       -> inner ks i' j  k' l  d     f
        Just (jj, kk) -> inner ks i' jj l  kk (- d) False
   where (i', j', k') = (i+1, j-1, k+d)

transL :: Ord a => STArray s Int a -> Int -> Int -> Int -> Int -> ST s (Maybe (Int, Int))
transL ks i j k d = do
  ki <- readArray ks i
  writeArray ks k ki
  ki' <- readArray ks i'
  if ki <= ki'
    then return Nothing
    else do p <- flushR ks j k' d
            return $ Just p
  where (i', k') = (i+1, k+d)

flushR :: Ord a => STArray s Int a -> Int -> Int -> Int -> ST s (Int, Int)
flushR ks j k d = do
  kj <- readArray ks j
  writeArray ks k kj
  kj' <- readArray ks j'
  if kj <= kj'
    then flushR ks j' k' d
    else return (j', k')
  where (j', k') = (j-1, k+d)

transR :: Ord a => STArray s Int a -> Int -> Int -> Int -> Int -> ST s (Maybe (Int, Int))
transR ks i j k d = do
  kj <- readArray ks j
  writeArray ks k kj
  kj' <- readArray ks j'
  if kj <= kj'
    then return Nothing
    else do p <- flushL ks i k' d
            return $ Just p
  where (j', k') = (j-1, k+d)

flushL :: Ord a => STArray s Int a -> Int -> Int -> Int -> ST s (Int, Int)
flushL ks i k d = do
  ki <- readArray ks i
  writeArray ks k ki
  ki' <- readArray ks i'
  if ki <= ki'
    then flushL ks i' k' d
    else return (i', k')
  where (i', k') = (i+1, k+d)

--------------------------------------------------------------------------------

main = do
  n  <- head <$> getInts
  hs <- getInts
  putStrLn . intercalate " " . map show . mergeSort n $ hs
