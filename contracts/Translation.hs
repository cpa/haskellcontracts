module Translation where

import qualified Haskell as H
import qualified FOL as F
import FOL (MetaFormula(..))
import Control.Monad.State
import Data.List (partition)
import Data.Char (toUpper)
import Control.Applicative

type Fresh = State TransState
data TransState = S { prefix  :: String -- the prefix of our fresh variables
                    , count   :: Int    -- a counter for the suffix of our fresh variables
                    , arities :: [H.Type H.Variable]} -- The arities of functions/data constructors in the program, which should be read-only

-- Utilities
------------

-- Define constants in one place: more concise code and easier to
-- change their definitions.
[false,true] = map (F.Var . F.Regular) ["'False'","'True'"]
unr = F.Var $ F.UNR
bad = F.Var $ F.BAD

-- Generate a fresh name.  Only used in one place :P
fresh :: Fresh F.Term
fresh = do
  s <- get
  let k = count s
  put $ s {count = k + 1}
  return $ makeVar (prefix s ++ show k)

-- Convert an FOL Var to a Haskell Var.
fVar2HVar (F.Var (F.Regular x)) = H.Var x

-- Make a TPTP variable from a string.
makeVar (c:cs) = F.Var $ F.Regular $ toUpper c : cs

-- Make k distinction variables s_1 ... s_k
makeVars k s = map (makeVar . (s++) . show) [1..k]

-- Make a selector function name.
makeSel k d = "sel_"++show k++"_"++unquote d where
  -- Strip single quotes.
  --
  -- XXX: maybe better to postpone any special handling of constructors
  -- in the parser.  I.e., *not* have single quotes yet at this point.
  unquote ('\'':cs) = init cs

-- Expression translation
-------------------------

eTrans :: H.Expression -> Fresh F.Term
eTrans (H.Var v) = return $ (F.Var $ F.Regular v)
eTrans (H.App e1 e2) = do
  t1 <- eTrans e1
  t2 <- eTrans e2
  return $ F.App [t1,t2]
eTrans (H.FullApp f es) = do
  ts <- sequence $ map eTrans es
  return $ F.FullApp (F.Regular f) ts
eTrans H.BAD = return bad

-- Definition translation
-------------------------

dTrans :: H.Definition -> Fresh [F.Formula]
dTrans (H.Let f vs e) = do
  et <- eTrans $ sub e
  if null vs
  -- XXX: again, could be simpler to handle emtpy quantification
  -- later, in the TPTP file generation phase.  This is at least the
  -- second place with special treatment.
  then return $ [(F.Var $ F.Regular f) :=: et]
  else return $ [F.Forall vs' $ (F.FullApp (F.Regular f) vs') :=: et]
--                ,fptr1,fptr2,fptr3]
  where vs'  = map makeVar vs
        vs'H = map fVar2HVar vs'
        sub  = H.substs (zip vs'H vs)
-- XXX, TODO: add back f_ptr support.
{-
        -- fptri are equations defining functions relatively to their app counterparts.
        -- eg that app(app(f_ptr,x),y) = f(x,y)
        fptr1 = (F.Forall vs' $ (F.And [F.CF v | v <- vs']) :=>: F.CF (F.FullApp (F.Regular f) vs')) :<=>: (F.CF $ F.Var $ F.Regular (f++"_ptr"))
        fptr2 = F.Forall vs' $ (F.FullApp (F.Regular f) vs') :=: (F.App $ (F.Var . F.Regular) (f++"_ptr") : vs')
        fptr3 = F.Forall vs' $ (F.FullApp (F.Regular (f ++ "p")) vs') :=: (F.App $ (F.Var . F.Regular) (f++"p_ptr") : vs')
-}

dTrans (H.LetCase f vs e pes) = do
  let vs'  = map makeVar vs
      -- Uppercasified variables wrapped in Haskell constructors.
      vs'H = map fVar2HVar vs'
      sub  = H.substs (zip vs'H vs)
  -- Uppercasify any quantified vars in e before translation.
  et <- eTrans $ sub e
  ft <- eTrans $ H.Var f
  let zedify ei pi = foldl (\e (v,z) -> H.subst (fVar2HVar z) v e) ei (take (length (tail pi)) $ zip (tail pi) zs)
      arities = map (\p -> (head p, length $ tail p)) $ map fst pes :: [(String,Int)]
      zs = [F.Var $ F.Regular $ "Zdef" ++ show x | x <- [1..(foldl1 max [snd y | y <- arities])]]
  tpieis <- sequence [eTrans (zedify ei pi) | (pi,ei) <- pes]
  let eq1 = [(et :=: (F.FullApp (F.Regular $ head pi) (take (length pi - 1) [ z | (v,z) <- zip (tail pi) zs ]))) :=>: (F.FullApp (F.Regular f) vs' :=: tpiei) | ((pi,ei),tpiei) <- zip pes tpieis]
      -- XXX: this equation is not in the paper, altho it looks reasonable.
      eq2 = (et :=: bad) :=>: (F.FullApp (F.Regular f) vs' :=: bad)
      eq3 = (F.And $ (et :/=: bad):bigAndSel ) :=>: eq4
      eq4 = (F.FullApp (F.Regular f) vs' :=: unr)
      bigAndSel = [et :/=: (F.FullApp (F.Regular di) [F.FullApp (F.Regular $ makeSel i di) [et] | i <- [1..ai]]) | (di,ai) <- arities]
-- XXX, TODO: add back f_ptr support.
{-
      fptr1 = (F.Forall vs' $ (F.And [F.CF v | v <- vs']) :=>: F.CF (F.FullApp (F.Regular f) vs')) :<=>: (F.CF $ F.Var $ F.Regular (f++"_ptr"))
      fptr2 = F.Forall vs' $ (F.FullApp (F.Regular f) vs') :=: (F.App $ (F.Var . F.Regular) (f++"_ptr") : vs')
      fptr3 = F.Forall vs' $ (F.FullApp (F.Regular (f ++ "p")) vs') :=: (F.App $ (F.Var . F.Regular) (f++"p_ptr") : vs')
-}
  return $ [F.Forall (vs' ++ zs) $ F.And (eq1++[eq2,eq3])] -- ,fptr1,fptr2,fptr3]

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

cTrans e (H.Arr x c1 c2) = do
  -- XXX, HACK: the parser inserts "" for arrows with unlabeled
  -- arguments.  I think this the only place in the whole translation
  -- where we actually need fresh names :P
  x' <- if x == "" then fresh else return $ makeVar x
  let x'H = fVar2HVar x'
      c2' = H.substC x'H x c2
  [f1] <- cTrans x'H c1
  [f2] <- cTrans (H.App e x'H) c2'
  return $ [F.Forall [x'] (f1 :=>: f2)]

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
  f (d,a,_) =
    let xs = makeVars a "X"
    in F.Forall xs $ F.And [x :=: F.FullApp (F.Regular $ makeSel k d)
                                    [F.FullApp (F.Regular d) $ xs]
                           | (x,k) <- zip xs [1..a]]

-- Axiom: Term constructors have disjoint ranges (Phi_2 in paper).
phi_disjoint (H.Data _ dns) = map f [(a,b) | a <- dns, b <- dns, a < b] where
  f ((d1,a1,_),(d2,a2,_)) =
    let xs = makeVars a1 "X"
        ys = makeVars a2 "Y"
    in F.Forall (xs++ys) $
      (F.FullApp (F.Regular d1) xs) :/=: (F.FullApp (F.Regular d2) ys)

-- Axiom: Term constructors are CF (Phi_3 in paper).
phi_cf (H.Data _ dns) = map f dns where
  f (d,a,_) =
    let xs = makeVars a "X" in
    if xs /= []
    -- XXX: maybe better to handle the special cas of empty
    -- quantifications in the translation to a tptp file?
    then F.Forall xs $ (F.CF $ F.FullApp (F.Regular d) xs)
                       :<=>: (F.And [F.CF x | x <- xs])
    else F.CF $ F.Var $ F.Regular d

-- Axiom: Term constructors are total/lazy (Phi_4 in paper).
phi_total (H.Data _ dns) = map f dns where
  f (d,a,_) =
    let xs = makeVars a "X" in
    if xs /= []
    then F.Forall xs $
           F.FullApp (F.Regular d) xs :/=: unr
    else (F.Var $ F.Regular d) :/=: unr

-- Final translation
--------------------

isToCheck :: [H.Variable] -> H.DefGeneral -> Bool
isToCheck fs (H.Def (H.Let f _ _))         = f `elem` fs
isToCheck fs (H.Def (H.LetCase f _ _ _))   = f `elem` fs
isToCheck fs (H.ContSat (H.Satisfies f _)) = f `elem` fs
isToCheck _ _                              = False

trans :: H.Program -> [H.Variable] -> [F.Formula]
trans ds fs = evalState (go fs ((H.appify) ds)) (S "Z" 0 (H.arities ds))
  where go fs ds = do
          let (toCheck,regDefs) = partition (isToCheck fs) ds
              recSubst  = H.substs  recVars
              recSubstC = H.substsC recVars
              recVars = zip (map recVar fs) fs
              recVar x = H.Var $ x ++ "p"
          a <- arities <$> get
          regFormulae <- forM regDefs $ \d -> case d of
            H.DataType t                -> tTrans t
            H.Def d                     -> dTrans d
            H.ContSat (H.Satisfies x y) -> F.appifyF a <$> cTrans (H.Var x) y
          speFormulae <- forM toCheck $ \d -> case d of
            H.DataType t                 -> error "No contracts for datatypes yet!"
            H.Def (H.Let f xs e)         -> dTrans $ H.Let     f xs (recSubst e)
            H.Def (H.LetCase f xs e pes) -> dTrans $ H.LetCase f xs (recSubst e)
                                                       [(p,(recSubst e)) | (p,e) <- pes]
            H.ContSat (H.Satisfies x y)  -> do
              contP   <- F.appifyF a <$> cTrans (recVar x) (recSubstC y)
              notCont <- map F.Not <$> F.appifyF a <$> cTrans (H.Var x) y
              return $ notCont ++ contP
          return $ concat $ prelude : regFormulae ++ speFormulae
            where prelude = [F.Forall [f,x]
                             $ F.And [F.CF f, F.CF x]
                               :=>: (F.CF $ F.App [f, x])
                            ,F.Not $ F.CF bad
                            ,F.CF unr
                            ,false :/=: true
                            ,F.CF true
                            ,F.CF false
                            ,true  :/=: unr
                            ,false :/=: unr]
                  [f,x] = map makeVar ["F","X"]
