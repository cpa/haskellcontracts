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
               , toList
               , ok
               , okContract)
where

type Variable = String
type Constructor = String

-- TODO Integer not implemented
data Expression = Var Variable
                | Con Constructor
                | App Expression Expression
                | FullApp Variable [Expression]
                | Sat Expression Contract -- e `satisfies` c --> True iff e \in c
                | CF Expression
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
                  
data DataType = Data Variable [(Variable,Int,Contract)] -- Data constructors + arity + contract
              deriving (Eq,Show,Ord)

type Pattern = [Variable]

data Contract = AppC Variable Contract Contract -- x : c -> c'
              | Pair Contract Contract
              | Pred Variable Expression   -- {x:e}
              | And Contract Contract
              | Or  Contract Contract
              | Any
              deriving (Show,Eq,Ord)                  

apps xs = foldl1 App xs


subst :: Expression -> Expression -> Variable -> Expression -- e[x/y]
subst (Var v) x y | v == y = x
                  | otherwise  = Var v
subst (Con c) x y | c == y = x
                  | otherwise  = Con c
subst (App e1 e2) x y = App (subst e1 x y) (subst e2 x y)
subst (FullApp f es) x y = let Var x' = (subst (Var f) x y) in 
  FullApp x' $ map (\e -> subst e x y) es
subst BAD _ _ = BAD
subst (CF e) x y = CF (subst e x y)

substC :: Contract -> Expression -> Variable -> Contract
substC (AppC u c1 c2) x y = AppC u (substC c1 x y) (substC c2 x y) -- TODO and if u==y?
substC (Pair c1 c2) x y = Pair (substC c1 x y) (substC c2 x y)
substC (Pred u e) x y = if u/=y then Pred u (subst e x y) else (Pred u e)
--substC (And cs) x y = And $ map (\c -> substC c x y) cs
--substC (Or cs) x y = Or $ map (\c -> substC c x y) cs
substC Any _ _ = Any

ok :: Contract
ok = Pred "dummy" (Con "true")

okContract 0 = ok
okContract n = AppC "okDummy" (okContract $ n-1) ok


toList (AppC _ c1 c2) = toList c1 ++ toList c2
toList x = [x]