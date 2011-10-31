module Translation where

import qualified Haskell as H
import Haskell (Name,Named,MetaNamed(..),Expression,MetaExpression(..),Arity)
import qualified FOL as F
import FOL (MetaFormula(..))
import Control.Monad.State
import Data.List (partition)
import Data.Char (toUpper)
import Control.Applicative

type Fresh = State TransState
data TransState = S { prefix  :: String -- the prefix of our fresh variables
                    , count   :: Int    -- a counter for the suffix of our fresh variables
                    , arities :: [Arity]} -- The arities of functions/data constructors in the program, which should be read-only

-- Utilities
------------

-- Define constants in one place: more concise code and easier to
-- change their definitions.
[false,true,unr,bad] = map (F.Named . F.Con) ["False","True","UNR","BAD"]

-- Generate a fresh name.  Only used in one place :P
fresh :: Fresh F.Name
fresh = do
  s <- get
  let k = count s
  put $ s {count = k + 1}
  return (prefix s ++ show k)

-- Make k distinction variables s_1 ... s_k
makeVars k s = map ((s++) . show) [1..k]

-- Make a selector function name.
makeSel k d = "sel_"++show k++"_"++d where

{- Moved to Haskell.hs to avoid module import cyle -}
-- Make a name for abstract recursive occurences of a function
-- makeRec f = f ++ "_rec"
-- Make a name for the curried ("pointer") version of a function
-- makePtr f = f ++ "_ptr"

-- Expression translation
-------------------------

eTrans :: H.Expression -> Fresh F.Term
eTrans = return

-- Definition translation
-------------------------

dTrans :: H.Definition -> Fresh [F.Formula]
dTrans (H.Let f vs e) = do
  -- Mark quantified variables as such.
  let vsN = map (Named . QVar) vs
      sub = H.substs (zip vsN vs)
  eT <- eTrans $ sub e
  return [F.Forall vs $ (F.FullApp (Var f) vsN) :=: eT]
--                ,fptr1,fptr2,fptr3]
-- XXX, TODO: add back f_ptr support.
{-
        -- fptri are equations defining functions relatively to their app counterparts.
        -- eg that app(app(f_ptr,x),y) = f(x,y)
        fptr1 = (F.Forall vs' $ (F.And [F.CF v | v <- vs']) :=>: F.CF (F.FullApp (F.Regular f) vs')) :<=>: (F.CF $ F.Var $ F.Regular $ makePtr f)
        fptr2 = F.Forall vs' $ (F.FullApp (F.Regular f) vs') :=: (F.App $ (F.Var . F.Regular) (makePtr f) : vs')
        fptr3 = F.Forall vs' $ (F.FullApp (F.Regular (H.makeRec f)) vs') :=: (F.App $ (F.Var . F.Regular) (makePtr $ makeRec f) : vs')
-}

-- Recall that the patterns 'pes' has the form [([Variable],Expression)].
dTrans (H.LetCase f vs e pes) = do
  -- Mark quantified variables as such.
  let vsQ = map QVar vs
      vsN = map Named vsQ
      sub = H.substs (zip vsN vs)
  -- Uppercasify any quantified vars in e before translation.
  eT <- eTrans $ sub e
  --pes' <- map
  -- A Pattern is a [Var], e.g. 'Cons x xs' ==> ['Cons','x','xs'], so
  -- the 'tail' of a pattern is the variables.  The 'arities' below would
  -- be simpler if Pattern were (Var,[Var]), e.g. 'Cons x xs' ==> ('Cons',['x','xs']).
  let -- e.g. [('Cons',2),('Nil',0)]
      arities = [(c, length xs) | ((c,xs),_) <- pes] :: [(String,Int)]
      -- A list of "fresh" variables. Length chosen to be large enough
      -- to substitute for any constructors pattern variables.  We use
      -- fresh variables here since pattern variables could shadow
      -- arguments: e.g. 'f x = case x of K x -> x'. We translate this
      -- to 'f x = case x of K z -> z'.  The problem is that we
      -- generate an equation 'x = K z' for when the scrutinee 'x' is
      -- equal to 'K _'.
      --
      -- XXX: the free vars could be avoided by using the projections
      -- here: e.g. instead we make an equation 'x = K(pi^K_1 x)'. I
      -- have no idea if Equinox would be better or worse for it.
      --
      -- XXX, TODO: try the above alternate translation in terms of
      -- projections.
      zs = makeVars (maximum $ map snd arities) "Zdef"
      -- Variables wrapped in Haskell constructors.
      zsQ = map QVar zs
      zsN = map Named zsQ
      -- NB: must substitute pattern variables before definition variables,
      -- because definition variables bind in an enclosing scope.
      patternSub ((_,xs),ei) = sub $ H.substs (zip zsN xs) ei
  pesT <- sequence $ map (eTrans . patternSub) pes
      -- e = Ki x1 ... xni -> f(xs) = ei
  let eq1 = [(eT :=: F.FullApp (Con c) (take (length xs) zsN))
             :=>: (F.FullApp (Var f) vsN :=: peT)
            | (((c,xs),_),peT) <- zip pes pesT]
      -- XXX: this equation is not in the paper, altho it looks reasonable.
      eq2 = (eT :=: bad) :=>: (F.FullApp (Var f) vsN :=: bad)
      eq3 = (F.And $ (eT :/=: bad):bigAndSel ) :=>: eq4
      eq4 = (F.FullApp (Var f) vsN :=: unr)
      bigAndSel = [eT :/=: (F.FullApp (Con c) [F.FullApp (Proj i c) [eT] | i <- [1..a]]) | (c,a) <- arities]
-- XXX, TODO: add back f_ptr support.
{-
      fptr1 = (F.Forall vs' $ (F.And [F.CF v | v <- vs']) :=>: F.CF (F.FullApp (F.Regular f) vs')) :<=>: (F.CF $ F.Var $ F.Regular $ makePtr f)
      fptr2 = F.Forall vs' $ (F.FullApp (F.Regular f) vs') :=: (F.App $ (F.Var . F.Regular) (makePtr f) : vs')
      fptr3 = F.Forall vs' $ (F.FullApp (F.Regular (H.makeRec f)) vs') :=: (F.App $ (F.Var . F.Regular) (makePtr $ makeRec f) : vs')
-}
  return $ [F.Forall (vs ++ zs) $ F.And (eq1++[eq2,eq3])] -- ,fptr1,fptr2,fptr3]

-- Contract translation
-----------------------

cTrans :: H.Expression -> H.Contract -> Fresh [F.Formula]
cTrans e H.Any = return [Top]

cTrans e (H.Pred x u) =  do
  let  u' = H.subst e x u
  et' <- eTrans e
  ut' <- eTrans u'
  et <- eTrans e
  return $ [F.And $ [F.Or [(et :=: unr) ,F.And [bad :/=: ut' , ut' :/=: false]]]] -- The data constructor False.

cTrans e (H.Arr mx c1 c2) = do
  -- Parser inserts 'Nothing' for unnamed arrow arguments.
  x <- maybe fresh return mx 
  let xN = F.Named $ F.QVar x
      c2' = H.substC xN x c2
  [f1] <- cTrans xN c1
  [f2] <- cTrans (e H.:@: xN) c2'
  return $ [F.Forall [x] (f1 :=>: f2)]

cTrans e (H.And c1 c2) = do
  [f1] <- cTrans e c1
  [f2] <- cTrans e c2
  return $ [F.And [f1,f2]]

cTrans e (H.Or c1 c2) = do
  [f1] <- cTrans e c1
  [f2] <- cTrans e c2
  return $ [F.Or [f1,f2]]

cTrans e (H.CF) = do
  et <- eTrans e
  return $ [F.CF $ et]

-- Data decl translation
------------------------

-- The axioms phi_* have type :: H.DataType -> [F.Formula (F.Term F.Variable)]
tTrans :: H.DataType -> Fresh [F.Formula]
tTrans d = return $ concat [phi_project d,phi_disjoint d,phi_cf d,phi_total d]

-- Axiom: Term constructors are invertable (Phi_1 in paper).
phi_project (H.Data _ dns) = map f dns where
  f (c,a,_) =
    let xs = makeVars a "X"
        xsN = map (Named . QVar) xs
    in F.Forall xs $ F.And [x :=: F.FullApp (Proj k c)
                                    [F.FullApp (Con c) $ xsN]
                           | (x,k) <- zip xsN [1..a]]

-- Axiom: Term constructors have disjoint ranges (Phi_2 in paper).
phi_disjoint (H.Data _ dns) = map f $ zip dns (tail dns) where
  f ((c1,a1,_),(c2,a2,_)) =
    let xs = makeVars a1 "X"
        xsN = map (Named . QVar) xs
        ys = makeVars a2 "Y"
        ysN = map (Named . QVar) ys
    in F.Forall (xs++ys) $
      (F.FullApp (Con c1) xsN) :/=: (F.FullApp (Con c2) ysN)

-- Axiom: Term constructors are CF (Phi_3 in paper).
phi_cf (H.Data _ dns) = map f dns where
  f (c,a,_) =
    let xs = makeVars a "X"
        xsN = map (Named . QVar) xs
    in
    F.Forall xs $ (F.CF $ F.FullApp (Con c) xsN)
                  :<=>: (F.And [F.CF x | x <- xsN])

-- Axiom: Term constructors are total/lazy (Phi_4 in paper).
phi_total (H.Data _ dns) = map f dns where
  f (c,a,_) =
    let xs = makeVars a "X"
        xsN = map (Named . QVar) xs
    in
    F.Forall xs $
      F.FullApp (Con c) xsN :/=: unr

-- Final translation
--------------------

isToCheck :: [H.Variable] -> H.DefGeneral -> Bool
isToCheck fs (H.Def (H.Let f _ _))         = f `elem` fs
isToCheck fs (H.Def (H.LetCase f _ _ _))   = f `elem` fs
isToCheck fs (H.ContSat (H.Satisfies f _)) = f `elem` fs
isToCheck _ _                              = False

-- XXX, ???: "appification" is an optimization.  Do we need it? It
-- also makes the generated formulas simpler, but at the cost of
-- complicating the generation code.  Would like to see how much if
-- any it speeds up Equinox in practice.
trans :: H.Program -> [H.Variable] -> [F.Formula]
trans ds fs = evalState (go fs ((H.appify) ds)) (S "Z" 0 (H.arities ds))
  where go fs ds = do
          let (toCheck,regDefs) = partition (isToCheck fs) ds
              recSubst  = H.substs  recVars
              recSubstC = H.substsC recVars
              recVars = zip (map recVar fs) fs
              recVar = H.Named . H.Rec
          a <- arities <$> get
          regFormulae <- forM regDefs $ \d -> case d of
            H.DataType t                -> tTrans t
            H.Def d                     -> dTrans d
            H.ContSat (H.Satisfies x y) -> F.appifyF a <$> cTrans (H.Named $ H.Var x) y
          checkFormulae <- forM toCheck $ \d -> case d of
            H.DataType t                 -> error "No contracts for datatypes yet!"
            H.Def (H.Let f xs e)         -> dTrans $ H.Let     f xs (recSubst e)
            H.Def (H.LetCase f xs e pes) -> dTrans $ H.LetCase f xs (recSubst e)
                                                       [(p,(recSubst e)) | (p,e) <- pes]
            -- For contract satisfaction 'f ::: c', generate
            -- 1) an assumption that the recursive occurances 'f_rec' satisfy the contract
            -- 2) the negation of 'f ::: c'
            -- XXX, ???: should 'f' be allowed to occur in 'c' at all? The 'recSubstC' below
            -- indicates this is expected.
            H.ContSat (H.Satisfies x y)  -> do
              contRec <- F.appifyF a <$> cTrans (recVar x) (recSubstC y)
              -- XXX, ???: why not 'recSubstC y' instead of plain 'y'?
              -- XXX, ???: should the goal receive a different label? The TPTP
              -- format supports e.g. 'conjecture' for goals, vs 'axiom' for
              -- assumptions. Doe Equinox distinguish (nc vaguely remembers getting
              -- different output in an experiment)?
              notCont <- map F.Not <$> F.appifyF a <$> cTrans (H.Named $ H.Var x) y
              return $ notCont ++ contRec
          return $ concat $ prelude : regFormulae ++ checkFormulae
            where prelude = [F.Forall [f,x]
                             $ F.And [F.CF fN, F.CF xN]
                               :=>: (F.CF $ fN :@: xN)
                            ,F.Not $ F.CF bad
                            ,F.CF unr
                            ,false :/=: true
                            ,F.CF true
                            ,F.CF false
                            ,true  :/=: unr
                            ,false :/=: unr]
                  [f,x] = ["F","X"]
                  [fN,xN] = map (Named  . QVar) [f,x]
