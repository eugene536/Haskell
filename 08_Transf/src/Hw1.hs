{-#LANGUAGE TupleSections #-}
module Hw1 where

import           Data.Monoid
import           Control.Applicative
import           Control.Monad.Trans.Class (MonadTrans, lift)
import           Transformers              (EitherT(..), StateT(..), WriterT(..))


instance (Monad m) => Monad (StateT s m) where
    return a = StateT $ \s -> return (a, s);
    -- (>>=) :: StateT s m a -> (a -> StateT s m b) -> StateT s m b
    m >>= f  = StateT $ \s -> do
        (a, newS) <- runStateT m s
        runStateT (f a) newS
        

instance MonadTrans (StateT s) where
    -- lift :: Monad m => m a -> StateT s m a
    lift m = StateT $ \s -> (,s) <$> m

instance (Monoid w, Monad m) => Monad (WriterT w m) where
    return a = WriterT $ return (a, mempty)
    -- (>>=) :: WriterT w m a -> (a -> WriterT w m b) -> WriterT w m b
    m >>= f  = WriterT $ do
        (a, w)  <- runWriterT m
        (b, w2) <- runWriterT (f a)
        return (b, w <> w2)

instance (Monoid w) => MonadTrans (WriterT w) where
    lift = WriterT . ((, mempty) <$>)

instance Monad m => Monad (EitherT l m) where
    return  = EitherT . return . Right
    -- (>>=) :: EitherT e m a -> (a -> EitherT e m b) -> EitherT e m b
    m >>= f = EitherT $ do
        v <- runEitherT m
        case v of
            Left  m     -> return $ Left m
            Right value -> runEitherT $ f value

instance MonadTrans (EitherT l) where
    -- lift :: m a -> EitherT l m a
    lift = EitherT . (Right <$>)

