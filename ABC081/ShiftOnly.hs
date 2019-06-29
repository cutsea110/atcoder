{-# LANGUAGE ScopedTypeVariables #-}
module Main where
 
main :: IO ()
main = do
  len <- readLn
  xs :: [Int] <- (map read . words) <$> getLine
  print $ go 0 (take len xs)
  where
    go :: Int -> [Int] -> Int
    go n xs | all even xs = go (n+1) $ map (`div` 2) xs
            | otherwise   = n
