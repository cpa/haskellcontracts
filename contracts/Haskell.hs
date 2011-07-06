{-# LANGUAGE DeriveFunctor #-}

module Haskell ( Expression (..)
               , DataType (..)
               , Program (..)
               , Definition (..)
               , Pattern (..)
               , Contract (..)
               , DefGeneral (..)
               , ContSat (..)
               , apps
               , arities  
               , appifyExpr
               , appify
               , subst
               , substC
               , toList
               , ok
               , okContract)
where

type Variable = String
type Constructor = String

data Expression = Var Variable
                | App Expression Expression
                | FullApp Variable [Expression]
                | Sat Expression (Contract Expression) -- e `satisfies` c --> True iff e \in c
                | CF Expression
                | BAD
                deriving (Show,Eq,Ord)

type Program = [DefGeneral Expression]
data DefGeneral a = ContSat (ContSat a)
                  | Def (Definition a)
                  | DataType (DataType a)
                  deriving (Eq,Show,Ord,Functor)

data ContSat a = Satisfies Variable (Contract a)
               deriving (Show,Eq,Ord,Functor)
               
data Definition a = Let Variable [Variable] a
                  | LetCase Variable [Variable] a [(Pattern,a)]
                  deriving (Show,Eq,Ord,Functor)
                  
data (DataType a) = Data Variable [(Variable,Int,Contract a)] -- Data constructors + arity + contract
                  deriving (Eq,Show,Ord,Functor)
                       
type Pattern = [Variable]

data Contract a = AppC Variable (Contract a) (Contract a) -- x : c -> c'
                | Pred Variable a  -- {x:e}
                | And (Contract a) (Contract a)
                | Or  (Contract a) (Contract a)
                | Any
                deriving (Show,Eq,Ord,Functor)

apps xs = foldl1 App xs

arities [] = []
arities (Def d:ds) = go d:arities ds
  where go (Let f vs _) = (f,length vs)
        go (LetCase f vs _ _) = (f,length vs)
arities (DataType d:gs) = go d ++ arities gs
  where go (Data d vac) = [(v,a) | (v,a,c) <- vac]
arities (d:ds) = arities ds

appify p = map (\d -> fmap (appifyExpr a) d) p
  where a = arities p

appifyExpr a e = go a 1 e []
  where go a count g@(App (Var v) e) acc = case lookup v a of
          Just n -> if count == n 
                    then FullApp v (e':acc)
                    else apps (App (Var $ v ++ "_ptr") e':acc)
          Nothing -> apps (App (Var v) e':acc)
          where e' = go a 1 e []
        go a count g@(App e1 e2) acc = go a (count+1) e1 (acc++[go a 1 e2 []])
        go a count (CF e) acc = CF (go a 1 e [])
        go a count (FullApp v es) acc = FullApp v $ map (\e -> go a 1 e []) es
        go a count (Sat e c) acc = Sat (go a 1 e []) c
        go a count BAD acc = BAD
        go a count (Var v) acc = Var v

subst :: Expression -> Expression -> Variable -> Expression -- e[x/y]
subst (Var v) x y | v == y = x
                  | otherwise  = Var v
subst (App e1 e2) x y = App (subst e1 x y) (subst e2 x y)
subst (FullApp f es) x y = let Var x' = (subst (Var f) x y) in 
  FullApp x' $ map (\e -> subst e x y) es
subst BAD _ _ = BAD
subst (CF e) x y = CF (subst e x y)

substC :: (Contract Expression) -> Expression -> Variable -> (Contract Expression)
substC (AppC u c1 c2) x y = AppC u (substC c1 x y) (substC c2 x y) -- TODO and if u==y?
substC (Pred u e) x y = if u/=y then Pred u (subst e x y) else (Pred u e)
substC (And c1 c2) x y = And (substC c1 x y) (substC c2 x y)
substC (Or c1 c2) x y = Or (substC c1 x y) (substC c2 x y)
substC Any _ _ = Any

ok :: Contract Expression
ok = Pred "dummy" (Var "true")

okContract 0 = ok
okContract n = AppC "okDummy" (okContract $ n-1) ok


toList (AppC _ c1 c2) = toList c1 ++ toList c2
toList x = [x]