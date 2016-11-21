{-# LANGUAGE DeriveFunctor #-}

module Transformers
       ( StateT(..)
       , EitherT(..)
       , WriterT(..)
       , ReaderT(..)
       , MaybeT(..)
       ) where

import           Control.Applicative       (liftA2)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Control.Monad(liftM)

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }
    deriving (Functor)

instance (Functor m, Monad m) => Applicative (StateT s m) where
    pure a = StateT $ \ s -> return (a, s)
    StateT mf <*> StateT mx = StateT $ \ s -> do
        ~(f, s') <- mf s
        ~(x, s'') <- mx s'
        return (f x, s'')

instance (Monad m) => Monad (StateT s m) where
    return a = StateT $ \ s -> return (a, s)
    m >>= k  = StateT $ \ s -> do
        ~(a, s') <- runStateT m s
        runStateT (k a) s'
    fail str = StateT $ \ _ -> fail str

newtype EitherT l m a = EitherT { runEitherT :: m (Either l a) }
    deriving (Functor)

inEitherT0 :: m (Either l a) -> EitherT l m a
inEitherT0 = EitherT
inEitherT1 :: (m (Either l a) -> m (Either l b)) ->
              EitherT l m a -> EitherT l m b
inEitherT1 f = inEitherT0 . f . runEitherT
inEitherT2 :: (m (Either l a) -> m (Either l b) -> m (Either l c)) ->
              EitherT l m a -> EitherT l m b -> EitherT l m c
inEitherT2 f = inEitherT1 . f . runEitherT

instance Applicative f => Applicative (EitherT l f) where
  pure = inEitherT0 . pure . pure
  (<*>) = inEitherT2 . liftA2 . liftA2 $ id

newtype WriterT w m a = WriterT { runWriterT :: m (a, w) }
    deriving (Functor)

instance (Monoid w, Applicative m) => Applicative (WriterT w m) where
    pure a  = WriterT $ pure (a, mempty)
    f <*> v = WriterT $ liftA2 k (runWriterT f) (runWriterT v)
      where k ~(a, w) ~(b, w') = (a b, w `mappend` w')
      

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

liftReaderT :: m a -> ReaderT r m a
liftReaderT m = ReaderT (const m)

mapReaderT :: (m a -> n b) -> ReaderT r m a -> ReaderT r n b
mapReaderT f m = ReaderT $ f . runReaderT m

instance (Functor m) => Functor (ReaderT r m) where
    fmap f  = mapReaderT (fmap f)

instance (Applicative m) => Applicative (ReaderT r m) where
    pure    = liftReaderT . pure
    f <*> v = ReaderT $ \ r -> runReaderT f r <*> runReaderT v r

instance MonadTrans (ReaderT r) where
    lift   = liftReaderT

instance (Monad m) => Monad (ReaderT r m) where
    return   = lift . return
    m >>= k  = ReaderT $ \ r -> do
        a <- runReaderT m r
        runReaderT (k a) r
    fail msg = lift (fail msg)


newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }


mapMaybeT :: (m (Maybe a) -> n (Maybe b)) -> MaybeT m a -> MaybeT n b
mapMaybeT f = MaybeT . f . runMaybeT

instance MonadTrans MaybeT where
    lift = MaybeT . liftM Just

instance (Functor m) => Functor (MaybeT m) where
    fmap f = mapMaybeT (fmap (fmap f))

instance (Functor m, Monad m) => Applicative (MaybeT m) where
    pure = lift . return
    mf <*> mx = MaybeT $ do
        mb_f <- runMaybeT mf
        case mb_f of
            Nothing -> return Nothing
            Just f  -> do
                mb_x <- runMaybeT mx
                case mb_x of
                    Nothing -> return Nothing
                    Just x  -> return (Just (f x))

instance (Monad m) => Monad (MaybeT m) where
    return = lift . return

    x >>= f = MaybeT $ do
        v <- runMaybeT x
        case v of
            Nothing -> return Nothing
            Just y  -> runMaybeT (f y)

    fail _ = MaybeT (return Nothing)
