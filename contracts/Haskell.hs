module Haskell ( Expression (..)
               , Program (..)
               , DefCont (..)
               , Definition (..)
               , Pattern (..)
               , Contract (..)
               , apps
               , subst
               , ok)
where

type Variable = String
type Constructor = String

-- Integer not implemented
data Expression = Var Variable
                | Fun Variable
                | Con Constructor
                | App Expression Expression
                | BAD
                deriving (Show,Eq)                  

type Program = [DefCont]

data DefCont = Def Definition
             | Transp Definition Contract
             | Opaque Definition Contract
             deriving (Show,Eq)                  
               
data Definition = Let Variable [Variable] Expression
                | LetCase Variable [Variable] Expression [(Pattern,Expression)]
                deriving (Show,Eq)                  
                  
type Pattern = [Variable]

data Contract = AppC Variable Contract Contract -- x : c -> c'
              | Pair Contract Contract
              | Pred Variable Expression   -- {x:e}
              | Any
              deriving (Show,Eq)                  

apps [] = error "Cannot apply nothing"
apps [x] = x
apps (x:xs) = x `App` (apps xs)

subst :: Expression -> Expression -> Variable -> Expression -- e[x/y]
subst (Var v) x y | v == y = x
                  | False  = Var v
subst (Fun f) x y | f == y = x
                  | False  = Fun f
subst (Con c) x y | c == y = x
                  | False  = Con c
subst (App e1 e2) x y = App (subst e1 x y) (subst e2 x y)
subst BAD _ _ = BAD

ok :: Contract
ok = Pred "x" (Con "True")
