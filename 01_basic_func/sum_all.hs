module SumAll where

import Data.Char (ord)

stringSum :: String -> Int
stringSum = sum . map read . words

--------------------Advanced----------------------------

readAdvanced :: String -> Int
readAdvanced s@(f:tl)
    | f == '+'  = if head tl /= '-'
                  then read tl
                  else error "- after +"
    | otherwise = read s

stringSumAdvanced :: String -> Int
stringSumAdvanced = sum . map readAdvanced . words
