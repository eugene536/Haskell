module Instances where

import           Prelude hiding (Either (..))

data Either a b = Left a | Right b
    deriving Show

instance Functor (Either a) where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (Right b) = Right $ f b
    fmap _ (Left a)  = Left a

instance Applicative (Either a) where
    -- pure :: a -> f a
    -- <*>  :: f (a -> b) -> f a -> f b
    pure                    = Right
    (Right f) <*> (Right v) = Right (f v)
    (Left f) <*> _          = Left f
    _ <*> (Left v)          = Left v

instance Foldable (Either a) where
    -- foldr :: (a -> b -> b) -> b -> f a -> b
    foldr f z (Right v) = f v z

instance Traversable (Either a) where
    -- traverse :: (Applicative f) => (a -> f b) -> t a -> f (t b)
    traverse f (Right v) = Right <$> f v

