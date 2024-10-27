module Main where

import GHC.Real (toInteger)
import Control.Arrow (first)
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


main = do
  n:_ <- getInts
  xs <- getIntVec n
  print $ solve n xs

solve :: Int -> U.Vector Int -> Int
solve n xs = length [(xs U.! i1, xs U.! i2, xs U.! i3, xs U.! i4, xs U.! i5)
                    | i1 <- [   0..n-1]
                    , i2 <- [i1+1..n-1]
                    , i3 <- [i2+1..n-1]
                    , i4 <- [i3+1..n-1]
                    , i5 <- [i4+1..n-1]
                    , xs U.! i1 + xs U.! i2 + xs U.! i3 + xs U.! i4 + xs U.! i5 == 1000
                    ]
