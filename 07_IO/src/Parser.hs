module Parser
    ( Parser(..)
    , first
    , second ) where

import           Control.Applicative (Alternative (..))
import           Control.Monad       (liftM)

newtype Parser a = Parser
    { runParser :: String -> Maybe (a, String) }

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

second :: (c -> d) -> (a, c) -> (a, d)
second f (a, c) = (a, f c)

instance Functor Parser where
    -- fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser run) = Parser run'
        where run' s = liftM (first f) $ run s

instance Applicative Parser where
    -- pure :: a -> Parser a
    -- <*>  :: Parser (a -> b) -> Parser a -> Parser b
    pure v  = Parser $ \s -> Just (v, s)
    (Parser f) <*> (Parser a) = Parser run'
        where
            run' s =
                f s   >>= \(fun, rem) ->
                a rem >>= \(val, rem') -> return (fun val, rem')


instance Alternative Parser where
    -- empty :: Parser a
    -- <|>   :: Parser a -> Parser a -> Parser a
    empty = Parser $ const Nothing
    (Parser l) <|> (Parser r) = Parser run'
        where run' s = l s <|> r s


