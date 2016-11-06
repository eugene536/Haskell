module Bst where

import           TreePrinters(Tree(..))
import qualified TreePrinters
import Control.Monad.Writer

find :: Eq a => a -> Tree a -> Bool
find x Leaf = False
find x (Node v l r) 
    | x == v    = True
    | otherwise = find x l || find x r

insert :: Ord a => a -> Tree a -> Tree a
insert x Leaf = Node x Leaf Leaf
insert x t@(Node v l r)
    | x < v  = Node v (insert x l) r
    | x > v  = Node v l (insert x r)
    | x == v = t

delete :: Ord a => a -> Tree a -> Tree a
delete x Leaf = Leaf
delete x t@(Node v l r)
    | x < v  = Node v (delete x l) r
    | x > v  = Node v l (delete x r)
    | x == v = getRest t
    where
        getRight (Node v l Leaf) = (v, l)
        getRight (Node v l r)    = (nv, Node v l nt)
            where (nv, nt) = getRight r

        getRest (Node v Leaf Leaf) = Leaf
        getRest (Node v Leaf r)    = r
        getRest (Node v l Leaf)    = l
        getRest (Node v l r)       = Node nv nl r
            where (nv, nl) = getRight l

toList   :: Tree a -> [Maybe a]
toList Leaf = [Nothing]
toList (Node v l r) = [Just v] ++ toList l ++ toList r

fromList :: [Maybe a] -> Tree a
fromList [] = error "list must not be empty"
fromList t = res
    where 
        fromListHelper [] = error "incorrect list"
        fromListHelper (Nothing : tl) = (Leaf, tl)
        fromListHelper (Just v  : tl) = (Node v l r, tl_r)
            where
                (l, tl_l) = fromListHelper tl
                (r, tl_r) = fromListHelper tl_l
        p_res = fromListHelper t
        res 
            | null $ snd p_res = fst p_res
            | otherwise        = error "incorrect list 2"

t = Node 3 (Node 1 Leaf Leaf) $ Node 123 (Node 4 Leaf Leaf) Leaf
prt t = putStr $ TreePrinters.verticalPrint t

t2 = insert 1 $ insert (-100) $ insert 100 $ insert 33 $ insert 101 $ insert (-110) $ insert (-90) Leaf
t3 = insert (-90) $ insert (-110) $ insert 101 $ insert 33 $ insert 100 $ insert (-100) $ insert 1 Leaf

--prt $ insert (-90) $ delete (-90) $ delete (-900) $ delete 1000 $ delete 100 $ insert (-100) $ delete (-100) $ insert (-110) $ delete (-110) $ insert 33 $ delete 33 $ t3
