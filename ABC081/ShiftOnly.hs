module Main where
 
main :: IO ()
main = do
  _  <- readLn :: IO Int
  xs <- map read . words <$> getLine
  print $ go 0 xs
  where
    go :: Int -> [Int] -> Int
    go n xs | all even xs = go (n+1) $ map (`div` 2) xs
            | otherwise   = n
