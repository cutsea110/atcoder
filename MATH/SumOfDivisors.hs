module Main where

import Data.Char (isSpace)
import Data.List (unfoldr, foldl')
import qualified Data.ByteString.Char8 as C

type Parser a = C.ByteString -> Maybe (a, C.ByteString)

parseInt :: Parser Int
parseInt = C.readInt . C.dropWhile isSpace

getInts :: IO [Int]
getInts = unfoldr parseInt <$> C.getLine

main = do
  n:_ <- getInts
  print $ solve n

solve :: Int -> Int
solve n = foldl' f 0 [1..n]
  where
    f acc i = acc + sumOn i
    -- 等差数列の和を求める
    -- i の倍数は n までに 1i, 2i, 3i, ... と n `div` i 個ある
    -- これらがそれぞれの数の約数に含まれ、それぞれの数分の重みで寄与する
    -- 例えば i = 3 なら 3, 6, 9, ... となるが、これはそれぞれ 3, 6, 9 に約数として含まれ、
    -- かつそれぞれ 3, 6, 9 の重みで寄与するので単純に足し込む
    -- 等差数列の和の公式で計算すれば O(1) で求まる
    sumOn i = numOfTerms * (i + lastElem) `div` 2
      where numOfTerms = n `div` i      -- 項数
            lastElem   = i * numOfTerms -- 最後の項

