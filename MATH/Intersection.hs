module Main where

import GHC.Real (toInteger)
import Control.Arrow (first)
import Data.Char (isSpace)
import Data.List (unfoldr)
import qualified Data.ByteString.Char8 as C

type Parser a = C.ByteString -> Maybe (a, C.ByteString)

parseInt :: Parser Integer
parseInt = fmap (first toInteger) . C.readInt . C.dropWhile isSpace

parseInt2 :: Parser (Integer, Integer)
parseInt2 cs = case parseInt cs of
  Just (x, cs') -> case parseInt cs' of
    Just (y, cs'') -> Just ((x, y), cs'')
    Nothing -> Nothing
  Nothing -> Nothing

getInts :: IO [Integer]
getInts = unfoldr parseInt <$> C.getLine

getTuple :: IO (Integer, Integer)
getTuple = do
  (x:y:_) <- getInts
  return (x, y)

type Point = (Integer, Integer)
type Vector = (Integer, Integer)

vector :: Point -> Point -> Vector
vector (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

cross :: Vector -> Vector -> Integer
cross (x1, y1) (x2, y2) = x1 * y2 - x2 * y1

intersect :: Point -> Point -> Point -> Point -> Bool
intersect a b c d | p1 < 0 && p2 < 0 = True
                  | p1 == 0 && (between c a b || between d a b) = True
                  | p2 == 0 && (between a c d || between b c d) = True
                  | otherwise = False
  where
    p1 = cross ac ab * cross ad ab
    p2 = cross ca cd * cross cb cd
    
    ab = vector a b
    ac = vector a c
    ad = vector a d

    cd = vector c d
    ca = vector c a
    cb = vector c b

between :: Point -> Point -> Point -> Bool
between (w1, w2) (x1, x2) (y1, y2)
  = min x1 y1 <= w1 && w1 <= max x1 y1 && min x2 y2 <= w2 && w2 <= max x2 y2

main :: IO ()
main = do
  a <- getTuple
  b <- getTuple
  c <- getTuple
  d <- getTuple
  putStrLn $ if intersect a b c d then "Yes" else "No"
