module Main where

import Data.List

main :: IO ()
main = do
  xs <- getLine
  print $ maximum $ map calc $ trans $ sep xs

sep [] = [("", "")]
sep (x:xs) = concat [[(x:a, b), (a, x:b)] | (a, b) <- sep xs]

trans xs = filter valid [(reverse (sort l), reverse (sort r)) | (l, r) <- xs]
  where valid (a, b) = f a && f b
        f xs = not (null xs || isPrefixOf "0" xs)

calc (x, y) = read x * read y
