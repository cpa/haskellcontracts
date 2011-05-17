module Haskell ( Expression (..)
               , DataType (..)
               , Program (..)
               , Definition (..)
               , Pattern (..)
               , Contract (..)
               , DefGeneral (..)
               , ContSat (..)
               , apps
               , subst
               , substC
               , ok)
where

type Variable = String
type Constructor = String

-- TODO Integer not implemented
data Expression = Var Variable
                | Fun Variable
                | Con Constructor
                | App Expression Expression
                | BAD
                deriving (Show,Eq,Ord)          

type Program = [DefGeneral]
data DefGeneral = ContSat ContSat
                | Def Definition
                | DataType DataType
                deriving (Eq,Show,Ord)

data ContSat = Satisfies Variable Contract
             deriving (Show,Eq,Ord)                  
               
data Definition = Let Variable [Variable] Expression
                | LetCase Variable [Variable] Expression [(Pattern,Expression)]
                deriving (Show,Eq,Ord)                  
                  
data DataType = Data Variable [(Variable,Int)] -- Data constructors + arity
              deriving (Eq,Show,Ord)

type Pattern = [Variable]

data Contract = AppC Variable Contract Contract -- x : c -> c'
              | Pair Contract Contract
              | Pred Variable Expression   -- {x:e}
              | Any
              deriving (Show,Eq,Ord)                  

apps xs = foldl1 (\w x -> App w x) xs


subst :: Expression -> Expression -> Variable -> Expression -- e[x/y]
subst (Var v) x y | v == y = x
                  | otherwise  = Var v
subst (Fun f) x y | f == y = x
                  | otherwise  = Fun f
subst (Con c) x y | c == y = x
                  | otherwise  = Con c
subst (App e1 e2) x y = App (subst e1 x y) (subst e2 x y)
subst BAD _ _ = BAD

substC :: Contract -> Expression -> Variable -> Contract
substC (AppC u c1 c2) x y = AppC u (substC c1 x y) (substC c2 x y) -- TODO and if u==y?
substC (Pair c1 c2) x y = Pair (substC c1 x y) (substC c2 x y)
substC (Pred u e) x y = if u/=y then Pred u (subst e x y) else (Pred u e)
substC Any _ _ = Any

ok :: Contract
ok = Pred "dummy" (Con "True")
