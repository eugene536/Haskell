module TestEmployeeParser where

import           Data.Char (isAlpha, isDigit)
import           Parser    (Parser (..))

type Name  = String
type Phone = String

data Employee = Empl
    { name  :: Name
    , phone :: Phone
    } deriving Show
parseName :: Parser Name
parseName = Parser run
    where run s = let p@(name, rem) = span isAlpha s in
                  if null name
                  then Nothing
                  else Just p

parsePhone :: Parser Phone
parsePhone = Parser run
    where run s = let p@(phone, rem) = span isDigit s in
                  if null phone
                  then Nothing
                  else Just p

parseEmployee :: Parser Employee
parseEmployee = Empl <$> parseName <*> parsePhone
