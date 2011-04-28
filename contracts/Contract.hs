module Contract where

import Expr

data Contract = Pred Variable Expr -- {var|expr(var)}
              | App Contract Contract
              | Any
              deriving (Eq,Show)
dummy = Pred "x" (Var "f")

toList :: Contract -> [Contract]
toList Any = [Any]
toList c@(Pred _ _) = [c]
toList (Contract.App c1 c2)  = c1:(toList c2)

codomain = last . toList 