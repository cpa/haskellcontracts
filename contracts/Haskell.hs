{-# LANGUAGE DeriveFunctor  #-}

module Haskell where

import Data.Maybe (isJust)

type Variable = String
type Constructor = String

-- All this meta stuff is just here to allow me to derive functors for free.
type DataType = MetaDataType Expression
type Expression = MetaExpression Named
type DefGeneral = MetaDefGeneral Expression
type Program = [DefGeneral]
type Pattern = (Name, [Name])
type Contract = MetaContract Expression
type Definition = MetaDefinition Expression

type Name = String
-- | Things with names.  We don't include function "pointers" here
-- because pointerness is determined by use: regular application ':@:'
-- uses pointers, whereas full application 'FullApp' uses
-- non-pointers.
--
-- Key difference from old design: fullapplication, not regular
-- application, is the special case.
data MetaNamed a =
             Var a -- ^ Regular variable, including functions.
           | Con a -- ^ Constructor
           -- The rest are only relevant to FOL? Could use GADT tricks
           -- to enforce this.
           | Rec a -- ^ Recursive version of a function
           | Proj Int a -- ^ Projector for a term constructor.
           -- There is no 'Full' because full application is
           -- determined by context.
           deriving (Eq,Ord,Show,Functor)

type Named = MetaNamed Name

-- | Projector for Named
getName :: Named -> Name
getName (Var v) = v
getName (Con v) = v
getName (Rec v) = v
getName (Proj _ v) = v

data MetaExpression v = Named v
                      -- Regular application: f x y => f @ x @ y
                      | (MetaExpression v) :@: (MetaExpression v)
                      -- Full application: f x y => f(x,y).
                      | FullApp v [MetaExpression v]
                      deriving (Show,Eq,Functor,Ord)

data MetaDefGeneral a = ContSat (MetaContSat a)
                      | Def (MetaDefinition a)
                      | DataType (MetaDataType a)
                      | Import FilePath
                      deriving (Eq,Show,Functor,Ord)

-- No contracts for constructors! So, Name, not Named here.
data MetaContSat a = Satisfies Name (MetaContract a)
                   deriving (Show,Eq,Functor,Ord)
               
data MetaDefinition a = Let Name [Name] a
                      -- LetCase f xs e [(p_i,e_i)]_i ~ f xs = case e of [p_i -> e_i]_i
                      | LetCase Name [Name] a [(Pattern,a)]
                      deriving (Show,Eq,Functor,Ord)
                  
data MetaDataType a = Data Variable [(Variable,Int,MetaContract a)] -- Data constructors + arity + contract
                    deriving (Eq,Show,Functor,Ord)

data MetaContract a = Arr (Maybe Name) (MetaContract a) (MetaContract a)
                    -- ^ 'x : c1 -> c2', with 'x' optional.
                    | Pred Variable a  -- {x:e}
                    | And (MetaContract a) (MetaContract a)
                    | Or  (MetaContract a) (MetaContract a)
                    | CF
                    | Any -- XXX: 'Any' is just '{x:True}', yeah?
                    deriving (Show,Eq,Functor,Ord)

type Arity = (Name,Int)

apps xs = foldl1 (:@:) xs

-- returns the arities of data constructors and functions
arities :: Program -> [Arity]
arities ds = concatMap go ds
  where go (Def (Let f vs _))       = [(f,length vs)]
        go (Def (LetCase f vs _ _)) = [(f,length vs)]
        go (DataType (Data _ vacs)) = [(v,a) | (v,a,_) <- vacs]
        go _ = []


appify :: Program -> Program
appify p = map (fmap $ appifyExpr a) p 
  where a = arities p

-- XXX, TODO: rename
-- | Return the arity of a name
lookupT :: Named -> [Arity] -> Maybe Int
lookupT v as = (lookup . getName) v as where

-- takes a program and a list of arities for each definition
-- returns the same program but using full application wherever possible
appifyExpr :: [Arity] -> Expression -> Expression
appifyExpr a e = go e []
  -- 'a' is the arities, 'args' is the arguments to the enclosing applications.
  where go (n@(Named v) :@: e) args = case lookupT v a of
          Just k -> if length args' == k
                    then FullApp v args'
                    else apps (n : e' : args)
          Nothing -> apps (n : e' : args)
          where e' = go e []
                args' = e':args

        go (e1 :@: e2) args = go e1 (args++[go e2 []])

        -- There should be no enclosing applications in these case, so no args.
        go (FullApp v es) [] = FullApp v $ map (\e -> go e []) es
        go (n@(Named _)) [] = n

-- Bunch of substitution utility

-- | Perform many substitutions, rightmost first.
substs :: [(Expression, Name)] -> Expression -> Expression
substs subs e = foldr (uncurry subst) e subs

-- | 'subst e1 y e2' = e2[e1/y]
--
-- NB: only Var, and not Con or Rec, can be substituted for.  The idea
-- is that Con and Rec aren't variables in the usual sense: they refer
-- to particular defined names.
subst :: Expression -> Name -> Expression -> Expression
subst e y e'@(Named (Var v)) | v == y    = e
                             | otherwise = e'
subst _ _ e'@(Named _)       = e' -- Don't substitute for non-var names.
subst e y (e1 :@: e2)        = (subst e y e1) :@: (subst e y e2)
subst e y (FullApp f es)     = let Named f' = (subst e y (Named f))
                               in FullApp f' $ map (subst e y) es

substsC :: [(Expression,Variable)] -> Contract -> Contract
substsC subs c = foldr (uncurry substC) c subs

substC :: Expression -> Variable -> Contract -> Contract
substC x y (Arr u c1 c2) = Arr u (substC x y c1) (substC x y c2) -- TODO and if u==y the semantics aren't very clear.
substC x y (Pred u e)     = if u/=y then Pred u (subst x y e) else (Pred u e)
substC x y (And c1 c2)    = And (substC x y c1) (substC x y c2)
substC x y (Or c1 c2)     = Or (substC x y c1) (substC x y c2)
substC x y CF             = CF 
substC _ _ Any            = Any
