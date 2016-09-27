module BstMerge 
       ( Tree(..)
       , prt
       ) where

import           TreePrinters(Tree(..))
import           Bst hiding (fromList, toList)
import           Data.Function(on)
import           Data.Monoid((<>))

toList :: Tree a -> [a]
toList Leaf         = []
toList (Node v l r) = toList l ++ [v] ++ toList r

fromList :: [a] -> Tree a
fromList []  = Leaf
fromList arr = Node v (fromList l_arr) (fromList r_arr)
    where
        l_arr = take pos arr
        r_arr = drop (pos + 1) arr
        v     = arr !! pos
        pos   = length arr `div` 2

mergeUniq :: (Ord a) => [a] -> [a] -> [a]
mergeUniq l [] = l
mergeUniq [] r = r
mergeUniq l1@(x:xs) l2@(y:ys)
    | x < y  = x : mergeUniq xs l2
    | y < x  = y : mergeUniq l1 ys
    | x == y = x : mergeUniq xs ys

instance (Ord a) => Monoid (Tree a) where 
    mempty  = Leaf
    mappend = (fromList .) . mergeUniq `on` toList

instance Foldable Tree where
    foldr f z Leaf         = z
    foldr f z (Node k l r) = foldr f (f k $ foldr f z r) l

ex1 = foldMap fromList [[x] | x <- [1..15]]
ex2 = foldMap fromList [[x] | x <- [5..19]]
ex3 = ex1 <> ex2
ex4 = (foldr (+) 0 ex3) == (sum [1..19])




