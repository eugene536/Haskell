{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Map where

import qualified Set as S

class Map m k v where
    emptyMap :: m k v
    toList   :: m k v -> [(k, v)]
    find     :: k -> m k v -> Bool
    insert   :: (k, v) -> m k v -> m k v
    delete   :: k -> m k v -> m k v
    next     :: k -> m k v -> Maybe k
    fromList :: [(k, v)] -> m k v

newtype Pair a b = Pair (a, b)
    deriving (Show)

fstM (Pair (a, _)) = a
sndM (Pair (_, b)) = b
toP  (Pair p)      = p

instance Eq a => Eq (Pair a b) where
    Pair (a, _) == Pair (a1, _) = a == a1

instance Ord a => Ord (Pair a b) where
    Pair (a0, _) <= Pair (a1, _) = a0 <= a1

newtype MyMap k v = MyMap (S.Tree (Pair k v))
    deriving (Show)
toS (MyMap s) = s

instance Ord k => Map MyMap k v where
    emptyMap = MyMap S.emptySet 
    toList   = map toP . S.toList . toS
    find   k = S.find (Pair (k, undefined)) . toS
    insert p = MyMap . S.insert (Pair p) . toS
    delete k = MyMap . S.delete (Pair (k, undefined)) . toS
    next   k = fmap fstM . S.next (Pair (k, undefined)) . toS
    fromList = MyMap . S.fromList . map Pair 

h = insert (1, "A") $ insert (2, "B") (insert (10, "Hello") (emptyMap  :: MyMap Int String))
n = next 1 (fromList $ toList h :: MyMap Int String)
n1 = next 10 (fromList $ toList h :: MyMap Int String)
-- S.prt $ fromList $ toList h :: MyMap Int String
