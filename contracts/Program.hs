module Program where
       
import Expr
import Contract 

data Program = Let Variable [Variable] Expr Contract
             | LetCase Variable [Variable] Expr [(Pattern,Expr)] Contract
             deriving (Eq,Show)
               
type Pattern = (Constructor,[Variable])

