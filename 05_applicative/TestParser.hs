module TestParser where

import           Control.Applicative (Alternative (..), liftA2, liftA3)
import           Control.Monad       (void)
import           Data.Char           (isUpper)
import           Parser              (Parser (..))
import           UsefulParsers       (char, posInt, satisfy)


abParser :: Parser (Char, Char)
abParser = liftA2 (,) (char 'a') $ char 'b'

abParser_ :: Parser ()
abParser_ = void abParser

intPair   :: Parser [Integer]
intPair = liftA3 (\x _ y -> [x, y]) posInt (char ' ') posInt

intOrUppercase :: Parser ()
intOrUppercase = toEmpty <$> posInt <|>
                 toEmpty <$> satisfy isUpper
                 where toEmpty x = ()

