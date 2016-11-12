module SafeHead where

safeHead :: [a] -> Maybe a
safeHead = foldr const Nothing . map Just


