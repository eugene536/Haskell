module InstancePair where

data Pair a b = Pair a b
    deriving Show

instance Functor (Pair v) where
    -- fmap :: (a -> b) -> Pair v a -> Pair v b
    fmap f (Pair a b) = Pair a (f b)

instance Monoid m => Applicative (Pair m) where
    -- pure :: a -> Pair m a
    -- <*>  :: Pair m (a -> b) -> Pair m a -> Pair m b
    pure = Pair mempty
    (Pair c f) <*> (Pair c' a) = Pair (c `mappend` c') (f a)

instance Foldable (Pair v) where
    -- foldMap :: (Monoid m) => (a -> m) -> Pair v a -> m
    foldMap f (Pair _ a) = f a

instance Traversable (Pair v) where
    -- traverse :: Applicative f => (a -> f b) -> Pair v a -> f (Pair v b)
    traverse f (Pair v a) = Pair v <$> f a
