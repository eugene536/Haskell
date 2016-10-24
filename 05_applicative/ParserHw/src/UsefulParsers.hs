module UsefulParsers
    ( satisfy
    , char
    , posInt
    , zeroOrMore
    , oneOrMore
    , spaces
    , ident ) where

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

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore  :: Parser a -> Parser [a]
oneOrMore p = liftA2 (++) ((:[]) <$> p) (zeroOrMore p)

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident  :: Parser String
ident = liftA2 (:) alphaParser $ zeroOrMore $ alphaParser <|> digitParser
    where
        alphaParser = satisfy isAlpha
        digitParser = satisfy isDigit
