{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Set
       ( Tree(..)
       , Set(..)
       , prt
       ) where

import           BstMerge
import           Data.Monoid((<>))

class Set s a where
    emptySet :: s a
    toList   :: s a -> [a]
    find     :: a -> s a -> Bool
    insert   :: a -> s a -> s a
    delete   :: a -> s a -> s a
    next     :: a -> s a -> Maybe a
    fromList :: [a] -> s a

instance Ord a => Set Tree a where
    emptySet = mempty
    toList   = foldr (:) []
    find   x = or . map (== x) . toList
    insert x = (<>) $ Node x Leaf Leaf
    delete x = fromList . filter (/= x) . toList 

    next x t = if null gr
               then Nothing 
               else Just $ head gr
        where gr = filter (> x) $ toList t

    fromList = foldMap (\x -> Node x Leaf Leaf) 

t = Node 3 (Node 1 Leaf Leaf) $ Node 123 (Node 4 Leaf Leaf) Leaf
t2 = insert 1 $ insert (-100) $ insert 100 $ insert 33 $ insert 101 $ insert (-110) $ insert (-90) Leaf
t3 = insert (-90) $ insert (-110) $ insert 101 $ insert 33 $ insert 100 $ insert (-100) $ insert 1 Leaf
