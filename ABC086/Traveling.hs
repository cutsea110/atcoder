module Main where

import Control.Monad (replicateM)

reachable :: [(Int,Int,Int)] -> Bool
reachable ps = and $ zipWith reach ps (tail ps)

reach :: (Int, Int, Int) -> (Int, Int, Int) -> Bool
reach (t0,x0,y0) (t1,x1,y1) = accessible && sameParity
  where
    tick = t1 - t0
    shortest = abs (x1 - x0) + abs (y1 - y0)
    accessible = shortest <= tick
    sameParity = even (tick - shortest)

readTriple :: IO (Int, Int, Int)
readTriple = do
  [t,x,y] <- map read . words <$> getLine
  return (t,x,y)

main :: IO ()
main = do
  n <- readLn
  ps <- replicateM n readTriple
  putStrLn $ if reachable ((0,0,0):ps) then "Yes" else "No"
