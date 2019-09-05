module Main where

main :: IO ()
main = do
  _n <- (read :: String -> Int) <$> getLine
  hs <- fmap (read :: String -> Int) . words <$> getLine
  print $ fst $ solve hs

solve hs = foldr acc (0,0) $ zipWith (>=) hs (tail hs)
  where
    acc a (m, c) = if a then (max m (c+1), c+1) else (max m c, 0)
