{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Coins where
import           Data.Monoid
import           Data.Typeable
import           Debug.Trace   (trace)

newtype Coin color = Coin
                    { getCoin :: Int
                    } deriving (Num, Show)
data Blue
data Red

createCoin :: color -> Int -> Coin color
createCoin _ = Coin

c1 = createCoin (undefined::Blue) 10
c2 = Coin 5 :: Coin Red

instance Monoid (Coin a) where
    mempty  = 0
    mappend = (+)

getInnerType :: Coin a -> a
getInnerType _ = undefined

compCoins :: (Typeable a, Typeable b) => Coin a -> Coin b -> Ordering
compCoins x y = compare l r
    where
        l = show $ typeOf $ getInnerType x
        r = show $ typeOf $ getInnerType y
