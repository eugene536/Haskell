module TestParser where

import Parser(Parser(..))
import Control.Applicative(liftA2, liftA3, Alternative(..))
import Data.Char(isDigit, isUpper)

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

abParser :: Parser (Char, Char)
abParser = liftA2 (,) (char 'a') $ char 'b'

abParser_ :: Parser ()
abParser_ = const () <$> abParser

intPair   :: Parser [Integer]
intPair = liftA3 (\x _ y -> [x, y]) posInt (char ' ') posInt

intOrUppercase :: Parser ()
intOrUppercase = toEmpty <$> posInt <|> 
                 toEmpty <$> (satisfy isUpper)
                 where toEmpty x = ()

