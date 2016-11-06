module Expr where

import           Control.Monad.Reader
import           Data.Map.Strict      (Map, (!))
import qualified Data.Map.Strict      as Map

type VarType = String
type ValType = Int

class Evaluable e where
    eval :: e -> Reader (Map VarType ValType) ValType

newtype Lit = Lit ValType

newtype Var = Var VarType

data Add a b = Add a b

data Sub a b = Sub a b

data Mul a b = Mul a b

data Assign c = Assign VarType ValType c

instance Evaluable Lit where
    eval (Lit x) = return x

instance Evaluable Var where
    eval (Var c) = asks $ \m -> m ! c

instance (Evaluable a, Evaluable b) => Evaluable (Add a b) where
    eval (Add a b) = asks $ \m -> runReader (eval a) m + runReader (eval b) m

instance (Evaluable a, Evaluable b) => Evaluable (Mul a b) where
    eval (Mul a b) = asks $ \m -> runReader (eval a) m * runReader (eval b) m

instance (Evaluable a, Evaluable b) => Evaluable (Sub a b) where
    eval (Sub a b) = asks $ \m -> runReader (eval a) m - runReader (eval b) m

instance (Evaluable c) => Evaluable (Assign c) where
    eval (Assign c v ctxt) = local (Map.insert c v) (eval ctxt)

val = runReader (eval $ Var "x" `Add` Lit 20 `Mul` Var "y") (Map.fromList [("y", 20), ("x", 10), ("z", 200)])
val2 = runReader (eval $ Var "x" `Add` (Lit 3 `Mul` ("x" `Assign` 2 $ Var "x"))) (Map.fromList [("x", 1)])


