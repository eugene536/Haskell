{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

import           Prelude ((.), id, const, undefined, flip, seq)

class MonadFish m where
    returnFish :: a -> m a
    (>=>)      :: (a -> m b) -> (b -> m c) -> (a -> m c)

class MonadJoin m where
    returnJoin :: a -> m a
    join       :: m (m a) -> m a

class Monad m where
    return :: a -> m a

    (>>=) :: m a -> (a -> m b) -> m b

    (>>)  :: m a -> m b -> m b
    a >> b = a >>= \_ -> b

instance Monad     m => MonadFish m where 
    returnFish = return
    f >=> g    = (>>= g) . f

{-instance Monad     m => MonadJoin m where -}
    {-returnJoin = return-}
    {-join       = (>>= id)-}

instance MonadFish m => Monad     m where 
    return = returnFish
    m >>= f  = (id >=> f) m

instance MonadFish m => MonadJoin m where 
    returnJoin = returnFish
    join       = id >=> id

