{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Coins where
import Data.Monoid 
import Data.Typeable
import Debug.Trace (trace)

newtype Coin color = Coin 
                    { getCoin :: Int } 
data Blue
data Red

createCoin :: color -> Int -> Coin color
createCoin _ = Coin

c1 = createCoin (undefined::Blue) 10
c2 = Coin 5 :: Coin Red

instance Num (Coin a) where
    (Coin a) + (Coin b) = Coin (a + b)
    (Coin a) * (Coin b) = Coin (a * b)
    abs (Coin a)        = Coin (abs a)
    signum (Coin a)     = Coin (signum a)
    fromInteger a       = Coin (fromInteger a)
    negate (Coin a)     = Coin (negate a)

instance Monoid (Coin a) where
    mempty                    = Coin 0
    mappend (Coin a) (Coin b) = Coin (a + b)


getInnerType :: Coin a -> a
getInnerType _ = undefined
-- compCoins :: (Typeable a, Typeable b) => Coin a -> Coin b -> Ordering
compCoins x y = compare l r
    where
        l = show $ typeOf $ getInnerType x
        r = show $ typeOf $ getInnerType y
