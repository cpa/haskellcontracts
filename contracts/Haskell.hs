{-# LANGUAGE DeriveFunctor  #-}

module Haskell where

type Variable = String
type Constructor = String

-- All this meta stuff is just here to allow me to derive functors for free.

type DataType = MetaDataType Expression
type Expression = MetaExpression Variable
type DefGeneral = MetaDefGeneral Expression
type Program = [DefGeneral]
type Pattern = [Variable]
type Contract = MetaContract Expression



data MetaExpression v = Var v
                      | App (MetaExpression v) (MetaExpression v)
                      | FullApp Variable [MetaExpression v]
                      | BAD
                      deriving (Show,Eq,Functor,Ord)

data MetaDefGeneral a = ContSat (MetaContSat a)
                      | Def (MetaDefinition a)
                      | DataType (MetaDataType a)
                      deriving (Eq,Show,Functor,Ord)


data MetaContSat a = Satisfies Variable (MetaContract a)
                   deriving (Show,Eq,Functor,Ord)
               
data MetaDefinition a = Let Variable [Variable] a
                      | LetCase Variable [Variable] a [(Pattern,a)]
                      deriving (Show,Eq,Functor,Ord)
                  
data MetaDataType a = Data Variable [(Variable,Int,MetaContract a)] -- Data constructors + arity + contract
                    deriving (Eq,Show,Functor,Ord)
                       
data MetaContract a = AppC Variable (MetaContract a) (MetaContract a) -- x : c -> c'
                    | Pred Variable a  -- {x:e}
                    | And (MetaContract a) (MetaContract a)
                    | Or  (MetaContract a) (MetaContract a)
                    | CF
                    | Any
                    deriving (Show,Eq,Functor,Ord)



apps xs = foldl1 App xs

-- returns the arities of data constructors and functions
arities :: Program -> [(Variable,Int)]
arities x = go x >>= \(f,i) -> [(f,i),(f++"p",i)]
  where go [] = []
        go (Def d:ds) = go2 d:go ds
          where go2 (Let f vs _) = (f,length vs)
                go2 (LetCase f vs _ _) = (f,length vs)
        go (DataType d:gs) = go2 d ++ go gs
          where go2 (Data d vac) = [(v,a) | (v,a,c) <- vac]
        go (d:ds) = go ds


appify :: Program -> Program
appify p = map (fmap $ appifyExpr a) p 
  where a = arities p


-- takes a program and a list of arities for each definition
-- returns the same program but using full application wherever possible
appifyExpr a e = go a 1 e []
  where go a count g@(App (Var v) e) acc = case lookup v a of
          Just n -> if count == n 
                    then FullApp v (e':acc)
                    else apps (App (Var $ v ++ "_ptr") e':acc)
          Nothing -> apps (App (Var v) e':acc)
          where e' = go a 1 e []
        go a count g@(App e1 e2) acc = go a (count+1) e1 (acc++[go a 1 e2 []])
        go a count (FullApp v es) acc = FullApp v $ map (\e -> go a 1 e []) es
        go a count BAD acc = BAD
        go a count (Var v) acc = case lookup v a of
          Just 0 -> Var v
          Just n -> Var $ v ++ "_ptr"
          Nothing -> Var v


-- Bunch of substitution utility

substs :: [(Expression, Variable)] -> Expression -> Expression
substs [] e = e
substs ((x,y):xys) e = substs xys $ subst x y e

subst :: Expression -> Variable -> Expression -> Expression -- e[x/y]
subst x y (Var v) | v         == y = x
                  | otherwise = Var v
subst x y (App e1 e2)         = App (subst x y e1) (subst x y e2)
subst x y (FullApp f es)      = let Var x' = (subst x y (Var f)) in FullApp x' $ map (\e -> subst x y e) es
subst x y BAD                 = BAD


substsC :: [(Expression,Variable)] -> Contract -> Contract
substsC [] c = c
substsC ((x,y):xys) c = substsC xys $ substC x y c

substC :: Expression -> Variable -> Contract -> Contract
substC x y (AppC u c1 c2) = AppC u (substC x y c1) (substC x y c2) -- TODO and if u==y?
substC x y (Pred u e)     = if u/=y then Pred u (subst x y e) else (Pred u e)
substC x y (And c1 c2)    = And (substC x y c1) (substC x y c2)
substC x y (Or c1 c2)     = Or (substC x y c1) (substC x y c2)
substC x y CF             = CF 
substC _ _ Any            = Any


-- FIXME: it's needed to compile but utterly uselessx
ok :: Contract
ok = Pred "dummy" (Var "true")

okContract 0 = ok
okContract n = AppC "okDummy" (okContract $ n-1) ok


