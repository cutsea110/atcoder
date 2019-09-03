module Main where

main :: IO ()
main = do
  (a:b:_) <- map read . words <$> getLine
  print $ snd $ head $ dropWhile (\(x,y) -> b > x) [(n*a-n+1, n) | n <- [0..]]
