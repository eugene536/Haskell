module ZipN where

zipN :: ([a] -> b) -> [[a]] -> [b]
zipN f l = fEl:tEl 
    where
        fEl = f $ map head l
        tEl 
          | null (tl !! 0) = []
          | otherwise = zipN f tl
        tl = map tail l

