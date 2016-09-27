module Strip where

safeInit :: [a] -> Either String [a]
safeInit a
        | null a    = Left "empty list"
        | otherwise = Right $ init a

safeTail :: [a] -> Either String [a]
safeTail a
        | null a    = Left "empty list"
        | otherwise = Right $ tail a

strip :: [a] -> [a]
strip = eith2arr . safeInit . eith2arr . safeTail
    where
        eith2arr (Left  err) = []
        eith2arr (Right arr) = arr
          
