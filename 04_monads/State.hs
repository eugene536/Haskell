{-# LANGUAGE FlexibleContexts #-}

module State where

import           Prelude hiding (Monad (..))


newtype State s a = State
                    { fun :: s -> (a, s)
                    }

class Monad m where
    return :: a -> m a

    (>>=) :: m a -> (a -> m b) -> m b

    (>>)  :: m a -> m b -> m b
    a >> b = a >>= const b

instance Monad (State s) where
    return x = State (\s -> (x, s))
    s >>= f  = State newFun
        where
            funState = fun s
            newFun s' = fun (f v) oldS
                where (v, oldS) = funState s'

--x >>= return  == x    (+)
--return a >>= f == f a (+)

--data Random = Random { x :: Int }
    --deriving (Show)
--s' (Random x) = (x + 10, Random (x + 10))
--s = State s'

--f x = return (x + 10)

--rnd = Random 0

