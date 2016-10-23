module Instances where

newtype Identity a = Identity { runIdentity :: a }
    deriving Show

instance Functor Identity where
    -- fmap :: (a -> b) -> Identity a -> Identity b
    fmap f (Identity a) = Identity $ f a

instance Applicative Identity where
    -- pure :: a -> Identity a
    -- <*>  :: Identity (a -> b) -> Identity a -> Identity b
    pure = Identity
    (Identity f) <*> (Identity a) = Identity $ f a

instance Foldable Identity where
    -- foldr :: (a -> b -> b) -> b -> Identity a -> b
    foldr f z (Identity a) = f a z

instance Traversable Identity where
    -- (Foldable t, Functor t) => Traversable t
    -- traverse :: (Applicative f) => (a -> f b) -> t a -> f (t b)
    traverse f (Identity a) = Identity <$> f a

