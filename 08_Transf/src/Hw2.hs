{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.State.Class (MonadState (..))
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Data.Monoid
import           Transformers              (MaybeT (..), ReaderT (..), StateT (..))

--class Monad m => MonadState s m | m -> s where
    --get :: m s
    --put :: s -> m ()
    --state :: (s -> (a, s)) -> m a

instance (Monad m) => MonadState s (StateT s m) where
    -- get :: StateT s m s
    -- s -> m (s, s)
    get   = StateT $ \s -> return (s, s)
    put s = StateT $ \_ -> return ((), s)

instance (Monad m) => MonadState s (ReaderT s m) where
    -- get :: ReaderT s m s
    get   = ReaderT $ \s -> return s
    put s = ReaderT $ \s -> return ()

--instance (Monad m) => MonadState s (MaybeT m) where
--instance MonadState s m => MonadState s (MaybeT m) where
    ---- get :: MaybeT m a
    --get   = MaybeT $ return Nothing
    --put s = MaybeT $ return $ Just ()

instance MonadState s m => MonadState s (MaybeT m) where
    get = lift get
    put s = lift $ put s

