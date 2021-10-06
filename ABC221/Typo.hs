module Main where

import System.IO

main :: IO ()
main = do
  s <- getLine
  t <- getLine
  putStrLn $ if like s t 0 then "Yes" else "No"

like :: String -> String -> Int -> Bool
like [] [] _ = True
like [x] [y] _ = x == y
like (x1:xxs@(x2:xs)) (y1:yys@(y2:ys)) n
  | x1 == y1 = like xxs yys n
  | x1 /= y1 && x1 == y2 && x2 == y1 && n < 1 = like xs ys (n+1)
  | otherwise = False
