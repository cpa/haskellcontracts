module Translation where

-- XXX: nc did not do a very good job of imposing a fake distinction
-- between FOL and Haskell by e.g. using F.Var vs H.Var.
import qualified Haskell as H
import Haskell (Name,Named,MetaNamed(..),Expression,MetaExpression(..))
import qualified FOL as F
import FOL (MetaFormula(..))
--import HaskellTypes
import TranslationTypes

import Control.Monad.State
import Data.List (partition, intercalate)
import Data.Char (toUpper)
import Control.Applicative
import Control.Exception (assert)
import Control.Arrow (second)

--import Debug.Trace (traceShow)

-- Utilities
------------

-- Define constants in one place: more concise code and easier to
-- change their definitions.
[false,true] = map (F.Named . F.Con) ["False","True"]
[unr,bad] = map (F.Named . F.Var) ["unr","bad"]

-- Generate a fresh name.
fresh :: Fresh F.Name
fresh = do
  s <- get
  let k = count s
  put $ s {count = k + 1}
  return (prefix s ++ show k)

-- Make k distinction variables s_1 ... s_k
makeVars k s = map ((s++) . show) [1..k]

appify :: F.LabeledFormula -> Fresh F.LabeledFormula
appify f = do
  a <- gets arities
  return $ F.appify a f

-- Expression translation
-------------------------

eTrans :: H.Expression -> Fresh F.Term
eTrans = return

-- Definition translation
-------------------------

-- | Auxillary axioms relating 'f' to 'f_ptr'.
--
-- forall [x1, ..., xn]. f(x1, ..., xn) = f@x1@...@xn
--
-- NB: appification breaks this, so don't 'appify' it!
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
-- that the pointer is CF iff the full application is for CF
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

-- | Shorthand for a common idiom.
nv = Named . Var
-- | Return a fresh variable name, based on 'v', which does not occur
-- in 'fvs'.
makeFV fvs v = v' where
  v' = head $ filter (`notElem` fvs) (v:[v++show i | i <- [0..]])

-- | Make a list of fresh variables names.
makeFVs fvs vs = foldr go [] vs where
  --  We need to 'fold', not 'map', because each name generated is
  --  free when generating subsequent names.
  v `go` vs' = makeFV (vs'++fvs) v : vs'

-- | Translate a case expression.
--ceTrans :: H.Case -> Fresh [H.Case]
ceTrans (H.Base e) = H.Base <$> eTrans e
ceTrans (H.Case e pces) = H.Case <$> eTrans e <*> mapM pceTrans pces where
  pceTrans (p,ce) = do ceT <- ceTrans ce
                       return (p, ceT)

-- | Translate a definition.
dTrans :: H.Definition -> Fresh [F.LabeledFormula]
dTrans (H.Let f vs ce) = do
  rhs <- go vs ce
  let eq = F.Forall vs $ F.Min full :=>: rhs
  return $ map (F.LabeledFormula f)
               [eq, dPtr fV vs, dPtrRec fV vs]
 where
  vsN = map nv vs
  fV = Var f
  full = F.FullApp fV vsN

  -- Recursively translate case expressions, tracking free variables
  -- in 'fvs'.
  go fvs (H.Base e) = do
    eT <- eTrans e
    return $ full :=: e
  go fvs (H.Case e pces) = do
    eT <- eTrans e
    -- We use fresh variables here since pattern variables could
    -- shadow arguments: e.g. 'f x = case x of K x -> x'. We
    -- translate this to 'f x = case x of K z -> z'.  The problem is
    -- that we generate an equation 'x = K z' for when the scrutinee
    -- 'x' is equal to 'K _'.
    --
    -- XXX: the free vars could be avoided by using the projections
    -- here: e.g. instead we make an equation 'x = K(pi^K_1 x)'. I
    -- have no idea if Equinox would be better or worse for it.
    --
    -- XXX, TODO: try the above alternate translation in terms of
    -- projections.
    --
    -- Below we translate 'case e of [ci xsi -> ei]' to
    -- 
    --   min e /\ (
    --         -- case BAD of ... ==> BAD
    --         (e = bad /\ f xs = bad) \/
    --         -- case ci xsi of ... c1 xsi -> ei ... ==> ei
    --         (\/_i (exists xsi. ci xsi = e /\ f xs = [[ ei ]])) \/
    --         -- if e /= bad /\ e /= ci xsi for any i then unr
    --         (e /= bad /\
    --          (/\_i (forall xsi. ci xsi /= e)) /\
    --          f xs = unr))
    --
    -- where '[[ ei ]]' is a recursive occurrence of our self.  We
    -- freshen the 'xsi' to avoid capturing the 'xs'.
    --
    -- NB: It's *not* OK to simplify the UNR case to 'f xs = unr'.  I
    -- don't understand why it matters in proofs, but it does (I
    -- implemented the simpler version first, and tests started
    -- failing), and logically it's easy to see that the above is
    -- stronger:
    --
    --   (A /\ B) \/ (~A /\ C) -> (A /\ B) \/ C
    --
    -- but the converse implication does not hold.  For us we have,
    -- e.g., A := (e = BAD), B := (f xs = BAD), C := (f xs = UNR).

        -- 'conCase (p,ce)' returns '(e /= p, e = p /\ f xs = ce)',
        -- with the necessary translations and quantifications.
    let conCase ((c,vs),ce) = do
          let vs' = makeFVs fvs vs
              vs'N = map nv vs'
              fullC = F.FullApp (Con c) vs'N
          -- substitute the fresh pattern variables before recursing.
          ceT <- go (vs'++fvs) $ H.substsCE (zip vs'N vs) ce
          return (F.Forall vs' $        eT :/=: fullC
                 ,F.exists vs' $ F.And [eT :=: fullC, ceT])
    (nonConCases,conCases) <- unzip <$> mapM conCase pces
    let conCaseIneqs = [ F.Not eq | F.And (eq:_) <- conCases ]
        badCase = F.And [eT :=: bad, full :=: bad]
        unrCase = F.And [eT :/=: bad, F.And nonConCases, full :=: unr]
    return $ F.And [F.Min eT, F.Or $ [badCase] ++ conCases ++ [unrCase]]

-- Contract translation
-----------------------

data Variance = Plus | Minus
dual :: Variance -> Variance
dual Plus = Minus
dual Minus = Plus

cTrans :: Variance -> H.Expression -> H.Contract -> Fresh F.LabeledFormula
cTrans v e c = appify =<< (F.LabeledFormula "cTrans" <$> cTrans' v e c)

cTrans' :: Variance -> H.Expression -> H.Contract -> Fresh F.Formula
cTrans' _ e H.Any = return Top

cTrans' v e (H.Pred x p) =  do
  let  p' = H.subst e x p
  eT <- eTrans e
  p'T <- eTrans p'
  -- XXX, DESIGN CHOICE: could also do 'F.Or[p'T :=: unr, p'T :=: true]'
  let plain = F.And [F.Or [(eT :=: unr), F.And [bad :/=: p'T, p'T :/=: false]]]
  case v of
    Plus  -> return $ F.And [F.Min(eT),            F.Min(p'T)] :=>: plain
    Minus -> return $        F.Min(eT)  :=>: F.And [F.Min(p'T),      plain]

cTrans' v e (H.Arr mx c1 c2) = do
  -- Parser inserts 'Nothing' for unnamed arrow arguments.
  x <- maybe fresh return mx
  let xN = Named $ Var x
      app = e H.:@: xN -- 'e' should be eTrans'd ... except eTrans is id.
  f1 <- cTrans' (dual v) xN  c1
  f2 <- cTrans' v        app c2
  -- XXX, DESIGN CHOICE: need for 'min' constraint here depends on
  -- whether we have a 'min(f x) -> min(f)' axiom.
  return $ F.Forall [x] $ F.Min app :=>: (f1 :=>: f2)

cTrans' v e (H.And c1 c2) = do
  f1 <- cTrans' v e c1
  f2 <- cTrans' v e c2
  return $ F.And [f1,f2]

cTrans' v e (H.Or c1 c2) = do
  f1 <- cTrans' v e c1
  f2 <- cTrans' v e c2
  return $ F.Or [f1,f2]

cTrans' v e (H.CF) = do
  eT <- eTrans e
  -- XXX, DESIGN CHOICE: the 'min' constraint should be optional in
  -- the 'Minus' case.
  return $ case v of
    Plus  -> F.Min eT :=>: F.CF eT
    Minus -> F.CF eT

-- Data decl translation
------------------------

tTrans :: H.DataType -> Fresh [F.LabeledFormula]
tTrans d@(H.Data nm _) = do
           phi_cfd <- phi_cf d
           let phis = concat [phi_project d
                             ,phi_disjoint d
                             ,phi_total d
                             ,tPtr d]
                      ++ phi_cfd
           return $ map (F.LabeledFormula nm) phis
  where tPtr (H.Data _ cas) = [dPtr (Con c) (makeVars a "X") | (c,a,_) <- cas]

-- | Axiom: Term constructors are invertable (Phi_1 in paper).
--
-- XXX, DESIGN CHOICE: this axiom isn't used, although it could be
-- used to eliminate some quantified variables in other axioms.
phi_project (H.Data _ dns) = map f dns where
  f (c,a,_) =
    let xs = makeVars a "X"
        xsN = map (Named . Var) xs
        fullProj i e = F.FullApp (Proj i c) e
        fullProjK i = fullProj i [F.FullApp (Con c) xsN]
    -- XXX: the paper has 'min(c(xs))', but that can't be right ?
        projectCorrect = F.Forall xs $ F.And [F.Min (fullProjK i) :=>: (fullProjK i :=: x)
                                             | (x,i) <- zip xsN [1..a]]
        -- XXX,DESIGN CHOICE: no mins. This axiom slowed down Equinox,
        -- do we really want it?
        --
        -- forall y. (forall xs. K xs /= y) -> K_i y = UNR
        projectWrong i = F.Forall [y] $ (F.Forall xs $ yN :/=: F.FullApp (Con c) xsN)
                         :=>: (fullProj i [yN] :=: unr)
        y = "ZDEF"
        yN = Named . Var $ y
        projectWrongs = F.And [projectWrong i | i <- [1..a]]
    in projectCorrect --F.And [projectCorrect, projectWrongs]

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
    --in F.Forall (xs++ys) $ fullC1 :/=: full c2

-- Axiom: Term constructors are CF (Phi_3 in paper).
phi_cf (H.Data _ dns) = concat <$> mapM f dns where
  f (c,a,_) = do
    let xs = makeVars a "X"
        xsN = map (Named . Var) xs
        full = F.FullApp (Con c) xsN

        cN = Named $ Con c
        cfs = replicate (a+1) H.CF
        -- CF^a -> CF
        contract = foldr1 (H.Arr Nothing) cfs
       -- DESIGN CHOICE: appification.
    cfc <- F.appifyF =<< cTrans' Minus cN contract
    let phi = F.Forall xs $ F.Min full :=>:
                       (F.CF full
                       :=>: (F.And [F.CF x | x <- xsN]))
    return [phi,cfc]
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

-- XXX, ???: "appification" is an optimization.  Do we need it? It
-- also makes the generated formulas simpler, but at the cost of
-- complicating the generation code.  Would like to see how much if
-- any it speeds up Equinox in practice.
trans :: H.Program -> H.Program -> [F.LabeledFormula]
trans checks deps = evalState result startState
 where
  startState = S { prefix = "Z"
                 , count = 0
                 , arities = H.arities (checks++deps)
                 , getGoals = []
                 }
  result = do
    let
        -- Substitution functions for recursification.
        --
        -- XXX: We could generate a slightly better theory by
        -- only recursifying functions that are actually
        -- recursive.
        --
        -- XXX: this could be folded into the translation
        -- functions themselves, by allowing them to return a
        -- pair of goal and assumption formulas.  All Minus
        -- translations would produce only assumptions, but the
        -- Plus translation for contract satisfaction would also
        -- return an assumption on the recursification.  With
        -- that setup, the function translations could perform
        -- the recursification locally, assuming they had access
        -- to list of all recursive goal functions.  However,
        -- breaking it up into two phases is probably easier to
        -- understand.
        fs = map H.def2Name checks
        recSubst   = H.substs   recVars
        recSubstC  = H.substsC  recVars
        recSubstCE = H.substsCE recVars
        recVars = zip (map recVar fs) fs
        recVar = H.Named . H.Rec

        recursifyDef (H.Let f xs pces) = H.Let f xs (recSubstCE pces)

    -- Translate all the defs.  We treat 'checks' different than
    -- 'deps', e.g. we assume contracts in 'deps', but break contracts
    -- in 'checks' into an assumption of the recursive version and a
    -- goal of the regular version.
    depFormulae <- forM deps $ \d -> case d of
      H.DataType t                -> tTrans t
      -- assert: There shouldn't be any recursive occurrences of 'fs'
      -- in 'deps', unless the '-c FUN' option is in use.
      H.Def d                     -> assert (d' == d) $ dTrans d' where
        d' = recursifyDef d
      H.ContSat (H.Satisfies f c) -> (:[]) <$> cTrans Minus (H.Named $ H.Var f) c
    checkFormulae <- forM checks $ \d -> case d of
      H.DataType t                 -> tTrans t
      H.Def d                      -> dTrans $ recursifyDef d
      -- XXX, ???: should 'f' be allowed to occur in 'c' at all? The 'recSubstC' below
      -- indicates this is expected.
      H.ContSat (H.Satisfies f c)  -> do
        -- XXX, ???: why not 'recSubstC c' instead of plain 'c'?
        -- XXX, ???: should the goal receive a different label? The TPTP
        -- format supports e.g. 'conjecture' for goals, vs 'axiom' for
        -- assumptions. Doe Equinox distinguish (nc vaguely remembers getting
        -- different output in an experiment)?
        goal <- cTrans Plus (H.Named $ H.Var f) c
        modify (\s -> s { getGoals = goal : getGoals s })

        (:[]) <$> cTrans Minus (recVar f) (recSubstC c)
    -- The goal formula is the negated conjunction of all goals,
    -- because we do a refutation proof.
    goalFormula <- do
      goals <- gets getGoals
      let labels = map F.getLabel goals
          formulas = map F.getFormula goals
          label = intercalate "_" labels
          formula = F.Not $ F.And formulas
      return $ F.LabeledFormula label formula

    return . concat
           $ prelude : depFormulae ++ checkFormulae ++ [[goalFormula]]
      -- XXX, TODO: add 'min's in prelude
      where prelude = map (F.LabeledFormula "prelude") [
--                       cf1, cf2
--                      ,min
                       min

                      ,F.Not $ F.CF bad
                      ,F.CF unr
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
            min = F.Forall [f,x] $ F.Min (fN :@: xN) :=>: F.And[F.Min fN]
            [f,x] = ["F","X"]
            [fN,xN] = map (Named . Var) [f,x]
