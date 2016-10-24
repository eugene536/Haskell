module Instances where

import           Control.Applicative (liftA2)
import           Prelude             hiding (Either (..))

data Tree a = Node {
        rootLabel :: a,
        subForest :: [Tree a]
    } deriving Show

instance Functor Tree where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (Node label childs) = Node (f label) (fmap (fmap f) childs)

instance Applicative Tree where
    -- pure :: a -> f a
    -- <*>  :: f (a -> b) -> f a -> f b
    pure = flip Node []
    (Node f fs) <*> (Node v chls) = Node (f v) (zipWith id (fmap (<*>) fs) chls);

instance Foldable Tree where
    -- foldr :: (a -> b -> b) -> b -> t a -> b
    -- foldMap :: Monoid m => (a -> m) -> t a -> m
    foldMap f (Node x childs) = f x `mappend` foldMap (foldMap f) childs

instance Traversable Tree where
    -- traverse :: (Applicative f) => (a -> f b) -> Tree a -> f (Tree b)
    traverse f (Node label childs) = liftA2 Node (f label) $ traverse (traverse f) childs
