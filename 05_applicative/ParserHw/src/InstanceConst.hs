module InstanceConst where

newtype Const a b = Const { getConst :: a }
    deriving (Show)

instance Functor (Const a) where
    -- fmap :: (a -> b) -> f a -> f b
    fmap _ (Const v) = Const v

instance Monoid m => Applicative (Const m) where
     -- pure :: b -> (Const a b) b
     -- <*>  :: f (a -> b) -> f a -> f b
     -- <*>  :: Const m (a-> b) -> Const m a-> Const m b
    pure _ = Const mempty
    (Const a) <*> (Const v)  = Const (mappend a v)

instance Foldable (Const a) where
    -- foldMap :: Monoid m => (b -> m) -> (Const a) b -> m
    foldMap _ _ = mempty

instance Traversable (Const a) where
    -- traverse :: (Applicative f) => (c -> f b) -> Const a c -> f (Const a b)
    traverse f (Const v) = pure (Const v)
