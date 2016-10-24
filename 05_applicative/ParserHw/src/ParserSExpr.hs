module ParserSExpr(parseSExpr) where

import           Control.Applicative (Alternative (..))
import           Parser
import           UsefulParsers

type Ident = String

data Atom = N Integer | I Ident
    deriving Show

data SExpr = A Atom
           | Comb [SExpr]
    deriving Show

atom :: Parser Atom
atom = N <$> posInt <|>
       I <$> ident

parseSExpr :: Parser SExpr
parseSExpr =
    spaces *> (
        A <$> atom <|>
        (char '(' *> (Comb <$> oneOrMore parseSExpr) <* char ')')
    ) <* spaces
