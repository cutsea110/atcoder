module Main where

import GHC.Real (toInteger)
import Control.Arrow (first)
import Control.Monad (replicateM)
import Data.Char (isSpace)
import Data.List (unfoldr)
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

main = do
  n <- head <$> getInts

  print $ solve n

-- | N種類のアイテムがあって k 種類すでに入手済みのとき、次に新しいアイテムを入手するまでに試行回数の期待値は N/(N-k) 回です。(※1)
--   したがって k=0 から k=N-1 までの期待値の和を求めればよいので E = Σ_{k=0}^{N-1} N(N-k) = N * Σ_k=1}^{N} 1/k となります。
--
--  (※1) 全 N 種類のうち k 種類がすでに入手済みのとき、次に新しいアイテムを入手する確率は (N-k)/N でこれを p とします。
--        すでに持っているアイテムを引いてしまう確率は k/N ですので 1-p が失敗確率ということになります。
--        このとき新しいアイテムを引くまでガチャをまわす回数の期待値は幾何分布と呼ばれます。
--        幾何分布においてある事象が初めて起こるまでに掛かる試行回数の期待値は 1/p 回で与えられます。(※2)
--
--  (※2) 幾何分布の期待値の導出は以下の通りです。
--        成功する確率を p 失敗する確率を 1-p とし、初めて成功が出るまでの試行回数を確率変数 X とします。
--        つまり最初の成功までに試行を何回行ったかという回数が X です。
--        最初の成功が k 回目に起こる確率は (1-p)^(k-1) * p です。
--        つまり X = k となる確率は P(X = k) = (1-p)^(k-1) * p となります。
--        期待値 E(X) はすべての k についての総和なので E(X) = Σ_{k=1}^{∞} k P(X = k) つまり E(X) = Σ_{k=1}^{∞} k (1-p)^(k-1) * p となります。
--        この計算は簡単ではないので再帰的な式をたてて解きます。
--        E(X) を成功するまでの試行回数の期待値とすると E(X) = p * 1 + (1-p) * (1 + E(X)) となります。
--        最初の試行で成功する確率は p で 1 回で試行回数は 1 回で済むので期待値に対する寄与は p * 1 です。
--        最初の試行で失敗する確率は 1-p で、この場合、1 回の試行を行った後で、再び同じ期待値 E(X) を加算する必要があるので、寄与は (1-p) * (1 + E(X)) です。
--        この式を整理すると E(X) = 1/p となります。
--
solve :: Int -> Double
solve n = fromIntegral n * foldl (\acc x -> acc + 1.0 / fromIntegral x) 0.0 [1..n]
