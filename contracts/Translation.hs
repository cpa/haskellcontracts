module Translation where

-- XXX: nc did not do a very good job of imposing a fake distinction
-- between FOL and Haskell by e.g. using F.Var vs H.Var.
import qualified Haskell as H
import Haskell (Name,Named,MetaNamed(..),Expression,MetaExpression(..),Arity)
import qualified FOL as F
import FOL (MetaFormula(..))
import Control.Monad.State
import Data.List (partition)
import Data.Char (toUpper)
import Control.Applicative

--import Debug.Trace (traceShow)

type Fresh = State TransState
data TransState = S { prefix  :: String -- the prefix of our fresh variables
                    , count   :: Int    -- a counter for the suffix of our fresh variables
                    , arities :: [Arity]} -- The arities of functions/data constructors in the program, which should be read-only

-- Utilities
------------

-- Define constants in one place: more concise code and easier to
-- change their definitions.
[false,true,unr,bad] = map (F.Named . F.Con) ["False","True","UNR","BAD"]

-- Generate a fresh name.
fresh :: Fresh F.Name
fresh = do
  s <- get
  let k = count s
  put $ s {count = k + 1}
  return (prefix s ++ show k)

-- Make k distinction variables s_1 ... s_k
makeVars k s = map ((s++) . show) [1..k]

-- Expression translation
-------------------------

eTrans :: H.Expression -> Fresh F.Term
eTrans = return

-- Definition translation
-------------------------

-- | Auxillary axioms relating 'f' to 'f_ptr'.
--
-- forall [x1, ..., xn]. f(x1, ..., xn) = f@x1@...@xn
dPtr f vs = if null vs then Top else F.And [eq1Min]--,eq2Min]
  -- 'null' check above to avoid pointless 'f = f'.
  where vsN = map (Named . Var) vs
        fN = Named f
        apps = foldl (F.:@:)
        full = F.FullApp f vsN
        app = fN `apps` vsN

        eq1    = F.Forall vs $ full :=: app
        -- XXX, ???: are these min's right?
        eq1Min = F.Forall vs $
                   F.Or [F.Min full, F.Min app]
                     -- NB: once we know they're equal, we have min() for both.
                     :=>: full :=: app

-- XXX, ???: the old f_ptr support included eq2 below, asserting
-- that the poitner is CF iff the full application is for CF
-- arguments.  Is this sound? It's not sound in the model where
-- 'f x = UNR' when 'f x' is ill-typed: e.g., consider 'f = (BAD,BAD)'.
-- On the other hand, maybe the model can have
-- 'f x = UNR' when 'f x' is ill-typed *and* 'f' is CF, and 'f x = BAD'
-- otherwise?  UPDATE: but, this axiom is specialized to a
-- particular function, so it's type correct.  It's the general version
-- in the prelude that's particularly dubious.
--
-- XXX: this axiom causes three otherwise passing tests of timeout,
-- and three otherwise timing out tests to pass :P
        allCF = F.And [F.CF v | v <- vsN]
        fullCF = F.Forall vs $ allCF :=>: F.CF full
        eq2 = fullCF :<=>: F.CF fN
        fullCFMin = F.Forall vs $ allCF :=>: (F.Min full :=>: F.CF full)
        eq2Min = F.Min fN :=>: (fullCFMin :<=>: F.CF fN) -- XXX, ???: is this min right?

-- | Same as dPtr, but for recursive occurances of f.
dPtrRec f@(Var v) = dPtr $ Rec v

dTrans :: H.Definition -> Fresh [F.Formula]
dTrans (H.Let f vs e) = do
  eT <- eTrans e
  let vsN = map (Named . Var) vs
      fV = Var f
      full = F.FullApp fV vsN

      eq    = F.Forall vs $ full :=: eT
      eqMin = F.Forall vs $ F.Min full :=>: (full :=: eT)

  return [eqMin
         ,dPtr fV vs, dPtrRec fV vs]


-- Recall that the patterns 'pes' has the form [([Variable],Expression)].
dTrans (H.LetCase f vs e pes) = do
  eT <- eTrans e
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
      --
      -- XXX, TODO: use better "fresh" variables.
      zs = makeVars (maximum $ map snd arities) "Zdef"
      -- Variables wrapped in Haskell constructors.
      zsN = map (Named . Var) zs
      -- NB: must substitute pattern variables before definition variables,
      -- because definition variables bind in an enclosing scope.
      patternSub ((_,xs),ei) = H.substs (zip zsN xs) ei

      vsN = map (Named . Var) vs
      fV = Var f
  pesT <- sequence $ map (eTrans . patternSub) pes
      -- e = Ki x1 ... xni -> f(xs) = ei
  let full = F.FullApp (Var f) vsN

      eq1 = [(eT :=: F.FullApp (Con c) (take (length xs) zsN))
             :=>: (full :=: peT)
            | (((c,xs),_),peT) <- zip pes pesT]
      -- XXX: this equation is not in the paper, altho it looks reasonable.
      eq2 = (eT :=: bad) :=>: (full :=: bad)
      -- XXX: this equation is not in the paper, altho it's implied by
      -- the equations in the paper.
      eq3 = (F.And $ (eT :/=: bad):bigAndSel ) :=>: eq4
      eq4 = (full :=: unr)
      bigAndSel = [eT :/=: (F.FullApp (Con c) [F.FullApp (Proj i c) [eT] | i <- [1..a]])
                  | (c,a) <- arities]
      -- The main equations.
      -- XXX: in the paper, the eqs are conjoined under a single 'forall'.
      eqs = (eq1++[eq2,eq3])
      eqs'    = map (F.Forall (vs++zs)) eqs
      eqs'Min = map (\eq -> F.Forall (vs++zs) $ F.Min full :=>: F.And [F.Min eT,eq]) eqs
  return $ dPtr fV vs : dPtrRec fV vs : eqs'Min

-- Contract translation
-----------------------

data Variance = Plus | Minus
dual :: Variance -> Variance
dual Plus = Minus
dual Minus = Plus

cTrans :: Variance -> H.Expression -> H.Contract -> Fresh [F.Formula]
cTrans _ e H.Any = return [Top]

cTrans v e (H.Pred x p) =  do
  let  p' = H.subst e x p
  eT <- eTrans e
  p'T <- eTrans p'
  -- XXX, DESIGN CHOICE: could also do 'F.Or[p'T :=: unr, p'T :=: true]'
  let plain = F.And [F.Or [(eT :=: unr), F.And [bad :/=: p'T, p'T :/=: false]]]
  case v of
    Plus  -> return $ [F.And [F.Min(eT),            F.Min(p'T)] :=>: plain]
    Minus -> return $ [       F.Min(eT)  :=>: F.And [F.Min(p'T),      plain]]

cTrans v e (H.Arr mx c1 c2) = do
  -- Parser inserts 'Nothing' for unnamed arrow arguments.
  x <- maybe fresh return mx
  let xN = Named $ Var x
      app = e H.:@: xN -- 'e' should be eTrans'd ... except eTrans is id.
  [f1] <- cTrans (dual v) xN  c1
  [f2] <- cTrans v        app c2
  -- XXX, DESIGN CHOICE: need for 'min' constraint here depends on
  -- whether we have a 'min(f x) -> min(f)' axiom.
  return [F.Forall [x] $ F.Min app :=>: (f1 :=>: f2)]

cTrans v e (H.And c1 c2) = do
  [f1] <- cTrans v e c1
  [f2] <- cTrans v e c2
  return [F.And [f1,f2]]

cTrans v e (H.Or c1 c2) = do
  [f1] <- cTrans v e c1
  [f2] <- cTrans v e c2
  return [F.Or [f1,f2]]

cTrans v e (H.CF) = do
  eT <- eTrans e
  -- XXX, DESIGN CHOICE: the 'min' constraint should be optional in
  -- the 'Minus' case.
  return $ case v of
    Plus  -> [F.Min eT :=>: F.CF eT]
    Minus -> [F.CF eT]

-- Data decl translation
------------------------

-- The axioms phi_* have type :: H.DataType -> [F.Formula (F.Term F.Variable)]
tTrans :: H.DataType -> Fresh [F.Formula]
tTrans d = return $ concat [phi_project d
                           ,phi_disjoint d
                           ,phi_cf d
                           ,phi_total d
                           ,tPtr d]
  where tPtr (H.Data _ cas) = [dPtr (Con c) (makeVars a "X") | (c,a,_) <- cas]

-- | Axiom: Term constructors are invertable (Phi_1 in paper).
--
-- XXX, DESIGN CHOICE: this axiom isn't used, although it could be
-- used to eliminate some quantified variables in other axioms.
phi_project (H.Data _ dns) = map f dns where
  f (c,a,_) =
    let xs = makeVars a "X"
        xsN = map (Named . Var) xs
        full k = F.FullApp (Proj k c) [F.FullApp (Con c) xsN]
    -- XXX: the paper has 'min(c(xs))', but that can't be right ?
    in F.Forall xs $ F.And [F.Min (full k) :=>: (full k :=: x)
                           | (x,k) <- zip xsN [1..a]]

-- Axiom: Term constructors have disjoint ranges (Phi_2 in paper).
phi_disjoint (H.Data _ dns) = map f $ zip dns (tail dns) where
  f ((c1,a1,_),(c2,a2,_)) =
    let xs = makeVars a1 "X"
        xsN = map (Named . Var) xs
        ys = makeVars a2 "Y"
        ysN = map (Named . Var) ys
        fullC1 = F.FullApp (Con c1) xsN
        fullC2 = F.FullApp (Con c2) ysN
    -- XXX: are the 'min's right? Paper uses 'forall a.' here which
    -- means only one 'min'.
    in F.Forall (xs++ys) $ F.Or [F.Min fullC1, F.Min fullC2]
                           :=>: (fullC1 :/=: fullC2)

-- Axiom: Term constructors are CF (Phi_3 in paper).
phi_cf (H.Data _ dns) = map f dns where
  f (c,a,_) =
    let xs = makeVars a "X"
        xsN = map (Named . Var) xs
        full = F.FullApp (Con c) xsN
    in
    F.Forall xs $ F.Min full :=>:
                  (F.CF full
                  :<=>: (F.And [F.CF x | x <- xsN]))

-- Axiom: Term constructors are total/lazy (Phi_4 in paper).
phi_total (H.Data _ dns) = map f dns where
  f (c,a,_) =
    let xs = makeVars a "X"
        xsN = map (Named . Var) xs
        full = F.FullApp (Con c) xsN
    in
    F.Forall xs $ F.Min full :=>:
      F.And [full :/=: unr
            ,full :/=: bad]

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
            H.ContSat (H.Satisfies x y) -> F.appifyF a <$> cTrans Minus (H.Named $ H.Var x) y
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
              contRec <- F.appifyF a <$> cTrans Minus (recVar x) (recSubstC y)
              -- XXX, ???: why not 'recSubstC y' instead of plain 'y'?
              -- XXX, ???: should the goal receive a different label? The TPTP
              -- format supports e.g. 'conjecture' for goals, vs 'axiom' for
              -- assumptions. Doe Equinox distinguish (nc vaguely remembers getting
              -- different output in an experiment)?
              notCont <- map F.Not <$> F.appifyF a <$> cTrans Plus (H.Named $ H.Var x) y
              return $ notCont ++ contRec
          return $ concat $ prelude : regFormulae ++ checkFormulae
            -- XXX, TODO: add 'min's in prelude
            where prelude = [
--                             cf1, cf2
--                            ,min
                             min

                            ,F.Not $ F.CF bad
                            ,F.CF unr

                            -- XXX, MAYBE TODO: use 'dTrans' helper functions on
                            -- 'Data "Bool" [("True",0,undefined),("False",0,undefined)]'
                            -- instead of expanding them manually as below. NB: but don't
                            -- include 'phi_lazy'!
                            ,false :/=: true
                            ,F.CF true
                            ,F.CF false
                            ,true  :/=: unr
                            ,false :/=: unr
                            ,true :/=: bad
                            ,false :/=: bad
                            ]
                  -- forall f,x. cf(f) /\ cf(x) -> cf(f x)
                  cf1 = F.Forall [f,x]
                        $ F.And [F.CF fN, F.CF xN]
                          :=>: (F.CF $ fN :@: xN)
                  -- The "CF = CF -> CF" axiom.  Only makes sense when
                  -- the expression in question has an arrow type, but
                  -- hopefully it's sound in general.  This
                  -- corresponds to fptr1 above.
                  --
                  -- XXX: convince ourselves this is sound. See
                  -- discussion by ftpr1 above.
                  --
                  -- forall f. (forall x. cf(x) -> cf(f x)) <-> cf(f)
                  cf2 = F.Forall [f]
                        $ (F.Forall [x] $ (F.CF xN :=>: (F.CF $ fN :@: xN)))
                          :<=>: F.CF fN
                  -- XXX, DESIGN CHOICE: may not need this when
                  -- 'C[[x:c1 -> c2]]^v' always has 'min' constraints.
                  min = F.Forall [f,x] $ F.Min (fN :@: xN) :=>: F.And[F.Min fN,F.Min xN]
                  [f,x] = ["F","X"]
                  [fN,xN] = map (Named . Var) [f,x]
