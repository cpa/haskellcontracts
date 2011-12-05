{-# LANGUAGE DeriveFunctor  #-}

module Haskell (module Haskell, module Types.Haskell) where

import Data.Maybe (isJust)

import Types.Haskell
import Types.Translation (Arity)
import Generics (gfmap,GenericT)

-- | Projector for Named
getName :: Named -> Name
getName (Var v) = v
getName (Con v) = v
getName (Rec v) = v
getName (Proj _ v) = v
getName (Unroll _ v) = v

tls2Name :: TopLevelStatement -> Name
tls2Name (Def (Let f _ _))         = f
tls2Name (DataType (Data t _))     = t
tls2Name (ContSat (Satisfies f _)) = f

apps xs = foldl1 (:@:) xs

-- returns the arities of data constructors and functions
arities :: Program -> [Arity]
arities ds = concatMap go ds
  where go (Def (Let f vs _))       = [(f,length vs)]
        go (DataType (Data _ vacs)) = [(v,a) | (v,a,_) <- vacs]
        go _ = []

-- | "Appify" all 'Expression's in a term.
appify :: [Arity] -> GenericT
appify x = gfmap (appifyExpr x)

-- takes a program and a list of arities for each definition
-- returns the same program but using full application wherever possible
appifyExpr :: [Arity] -> Expression -> Expression
appifyExpr a e = go e []
  -- 'a' is the arities, 'args' is the arguments to the enclosing applications.
  where go (n@(Named v) :@: e) args = case lookup (getName v) a of
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
substs subs e    = foldr (uncurry subst) e subs
substsC subs c   = foldr (uncurry substC) c subs
substsCE subs ce = foldr (uncurry substCE) ce subs

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

substC :: Expression -> Name -> Contract -> Contract
-- The choice to treat 'u' as bound in 'c1' here is motivated by the
-- desire to simplify the predicate syntax: we currently write
-- 'x:{x:p} -> c2' to bind 'x' in 'c2' and constrain it by 'p'.  It
-- would be nicer to write 'x:{p} -> c2'.  Currently, it might make
-- more sense to treat 'u' as bound in 'c2' only.
substC x y a@(Arr mu c1 c2)
  | Just u <- mu
  , u == y                = a
  | otherwise             = Arr mu (substC x y c1) (substC x y c2)
substC x y (Pred u e)     = if u/=y then Pred u (subst x y e) else (Pred u e)
substC x y (And c1 c2)    = And (substC x y c1) (substC x y c2)
substC x y (Or c1 c2)     = Or (substC x y c1) (substC x y c2)
substC x y CF             = CF 
substC _ _ Any            = Any

-- | 'subst' for case expressions.
substCE e y (Base e') = Base $ subst e y e'
substCE e y (Case e' pces) = Case (subst e y e') (map substP pces) where
  -- Substitute into a case branch.  We stop if the pattern binds the
  -- variables we our substituting.
  substP pce@((c,vs),ce) = if y `elem` vs
                           then pce
                           else ((c,vs), substCE e y ce)



-- | Generate a Haskell function 'f' from a contract 'c', s.t. if 'f'
-- type checks then 'c' would too.
--
-- This is a simple hack.  We compile binders in contracts to lambda
-- binders, ignore CF and Any, and compile predicates to themselves.
contract2Haskell = go where
  go CF             = "()"
  go Any            = "()"
  go (Or c1 c2)     = "("++go c1++", "++go c2++")"
  go (And c1 c2)    = go (Or c1 c2)
  go (Pred x e)     = "(\\"++x++" -> "++goE e++")"
  go (Arr mx c1 c2) = "(\\"++maybe "_" id mx++" -> "++go (Or c1 c2)++")"

  -- haskell2Haskell :P
  goE (Named v)      = getName v
  goE (e1 :@: e2)    = "("++goE e1++" "++goE e2++")"
  goE (FullApp v es) = goE $ foldl (:@:) (Named v) es
