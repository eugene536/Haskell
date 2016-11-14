module UsefulParsers
    ( satisfy
    , char
    , posInt
    , zeroOrMore
    , oneOrMore
    , spaces
    , ident
    , optional ) where

import           Control.Applicative (Alternative (..), liftA2)
import           Data.Char           (isAlpha, isDigit, isSpace)
import           Parser              (Parser (..))

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
    where
        f [] = Nothing
        f (x:xs)
            | p x       = Just (x, xs)
            | otherwise = Nothing

char :: Char -> Parser Char
char c = satisfy (== c)

posInt :: Parser Integer
posInt = Parser f
    where
        f xs
            | null ns   = Nothing
            | otherwise = Just (read ns, rest)
            where (ns, rest) = span isDigit xs

optional :: Parser a -> Parser [a]
optional p = (:[]) <$> p <|> pure []

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore  :: Parser a -> Parser [a]
oneOrMore p = liftA2 (++) ((:[]) <$> p) (zeroOrMore p)

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident  :: Parser String
ident = oneOrMore $ alphaParser <|> digitParser <|> char '.'
    where
        alphaParser = satisfy isAlpha
        digitParser = satisfy isDigit
