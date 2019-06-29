module Main where

import Control.Arrow ((&&&))
import Data.List (isPrefixOf, reverse)

isDream, isDreamer, isErase, isEraser :: String -> Bool
restOfDream, restOfDreamer, restOfErase, restOfEraser :: String -> String
[(isDream,   restOfDream),
 (isDreamer, restOfDreamer),
 (isErase,   restOfErase),
 (isEraser,  restOfEraser)] = map (isPrefixOf . reverse &&& drop . length) ["dream", "dreamer", "erase", "eraser"]


exist :: String -> Bool
exist [] = True
exist sna
  | isDream sna = exist $ restOfDream sna
  | isDreamer sna = exist $ restOfDreamer sna
  | isErase sna = exist $ restOfErase sna
  | isEraser sna = exist $ restOfEraser sna
  | otherwise = False

main :: IO ()
main = do
  ans <- getLine
  putStrLn $ if exist (reverse ans) then "YES" else "NO"
