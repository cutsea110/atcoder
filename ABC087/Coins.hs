module Main where

calc :: (Int, Int, Int, Int) -> Int
calc (a, b, c, x) = sum [1| a'<-[0..a], b'<-[0..b], c'<-[0..c], 500*a'+100*b'+50*c' == x]
                        
main :: IO ()
main = do
  ps <- (,,,) <$> readLn <*> readLn <*> readLn <*> readLn
  print $ calc ps

