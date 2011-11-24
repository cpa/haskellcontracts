{-# LANGUAGE DeriveFunctor  #-}

module Haskell (module Haskell, module HaskellTypes) where

import Data.Maybe (isJust)

import HaskellTypes

-- | Projector for Named
getName :: Named -> Name
getName (Var v) = v
getName (Con v) = v
getName (Rec v) = v
getName (Proj _ v) = v

def2Name :: DefGeneral -> Name
def2Name (Def (Let f _ _))         = f
def2Name (DataType (Data t _))     = t
def2Name (ContSat (Satisfies f _)) = f

apps xs = foldl1 (:@:) xs

-- returns the arities of data constructors and functions
arities :: Program -> [Arity]
arities ds = concatMap go ds
  where go (Def (Let f vs _))       = [(f,length vs)]
        go (DataType (Data _ vacs)) = [(v,a) | (v,a,_) <- vacs]
        go _ = []

appify :: [Arity] -> Program -> Program
appify a p = map (fmap $ appifyExpr a) p

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
substC x y (Arr u c1 c2) = Arr u (substC x y c1) (substC x y c2) -- TODO and if u==y the semantics aren't very clear.
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
