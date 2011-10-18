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
[f,x,false,true] = map (F.Var . F.Regular) ["F","X","'False'","'True'"]
unr = F.Var $ F.UNR
bad = F.Var $ F.BAD

-- Generate a fresh name.  Only used in one place :P
fresh :: Fresh String
fresh = do
  s <- get
  let k = count s
  put $ s {count = k + 1}
  return $ makeVar (prefix s) ++ show k

-- Make a TPTP variable from a string.
--
-- NB: unlike makeVars, this does not wrap the result in 'F.Var . F.Regular'.
makeVar (c:cs) = toUpper c : cs

-- Make k distinction variables s_1 ... s_k
makeVars k s = map (F.Var . F.Regular . (makeVar s++) . show) [1..k]

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
  et <- eTrans e
  if null vs
  then return $ [(F.Var $ F.Regular f) :=: et]
  else return $ [F.Forall vvs $ (F.FullApp (F.Regular f) vvs)
                :=: et,fptr1,fptr2,fptr3]
  where vvs = map (F.Var . F.Regular) vs
        -- fptri are equations defining functions relatively to their app counterparts.
        -- eg that app(app(f_ptr,x),y) = f(x,y)
        fptr1 = (F.Forall vvs $ (F.And [F.CF v | v <- vvs]) :=>: F.CF (F.FullApp (F.Regular f) vvs)) :<=>: (F.CF $ F.Var $ F.Regular (f++"_ptr"))
        fptr2 = F.Forall vvs $ (F.FullApp (F.Regular f) vvs) :=: (F.App $ (F.Var . F.Regular) (f++"_ptr") : vvs)
        fptr3 = F.Forall vvs $ (F.FullApp (F.Regular (f ++ "p")) vvs) :=: (F.App $ (F.Var . F.Regular) (f++"p_ptr") : vvs)

dTrans (H.LetCase f vs e pes) = do
  et <- eTrans e
  ft <- eTrans $ H.Var f
  let zedify ei pi = foldl (\e (v,z) -> H.subst (H.Var $ extractVR z) v e) ei (take (length (tail pi)) $ zip (tail pi) zs)
      extractVR (F.Var (F.Regular v)) = v
      arities = map (\p -> (head p, length $ tail p)) $ map fst pes :: [(String,Int)]
      zs = [F.Var $ F.Regular $ "Zdef" ++ show x | x <- [1..(foldl1 max [snd y | y <- arities])]]
  tpieis <- sequence [eTrans (zedify ei pi) | (pi,ei) <- pes]
  let vvs = map (F.Var . F.Regular) vs
      eq1 = [(et :=: (F.FullApp (F.Regular $ head pi) (take (length pi - 1) [ z | (v,z) <- zip (tail pi) zs ]))) :=>: (F.FullApp (F.Regular f) vvs :=: tpiei) | ((pi,ei),tpiei) <- zip pes tpieis]
      eq2 = (et :=: bad) :=>: (F.FullApp (F.Regular f) vvs :=: bad)
      eq3 = (F.And $ (et :/=: bad):bigAndSel ) :=>: eq4
      eq4 = (F.FullApp (F.Regular f) vvs :=: unr)
      bigAndSel = [et :/=: (F.FullApp (F.Regular di) [F.FullApp (F.Regular $ makeSel i di) [et] | i <- [1..ai]]) | (di,ai) <- arities]
      fptr1 = (F.Forall vvs $ (F.And [F.CF v | v <- vvs]) :=>: F.CF (F.FullApp (F.Regular f) vvs)) :<=>: (F.CF $ F.Var $ F.Regular (f++"_ptr"))
      fptr2 = F.Forall vvs $ (F.FullApp (F.Regular f) vvs) :=: (F.App $ (F.Var . F.Regular) (f++"_ptr") : vvs)
      fptr3 = F.Forall vvs $ (F.FullApp (F.Regular (f ++ "p")) vvs) :=: (F.App $ (F.Var . F.Regular) (f++"p_ptr") : vvs)
  return $ [F.Forall (vvs ++ zs) $ F.And (eq1++[eq2,eq3]),fptr1,fptr2,fptr3]

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
  let c2' = H.substC (H.Var x') x c2
  [f1] <- cTrans (H.Var x') c1
  [f2] <- cTrans (H.App e (H.Var x')) c2'
  return $ [F.Forall [F.Var $ F.Regular $ x'] (f1 :=>: f2)]

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
              recVar x = x ++ "p"
          a <- arities <$> get
          regFormulae <- forM regDefs $ \d -> case d of
            H.DataType t                -> tTrans t
            H.Def d                     -> dTrans d
            H.ContSat (H.Satisfies x y) -> F.appifyF a <$> cTrans (H.Var x) y
          speFormulae <- forM toCheck $ \d -> case d of
            H.DataType t                 -> error "No contracts for datatypes yet!"
            H.Def (H.Let f xs e)         -> dTrans $ H.Let f xs (H.substs (zip (map (H.Var . recVar) fs) fs) e)
            H.Def (H.LetCase f xs e pes) -> dTrans $ H.LetCase f xs (H.substs (zip (map (H.Var . recVar) fs) fs) e) [(p,(H.substs (zip (map (H.Var . recVar) fs) fs) e)) | (p,e) <- pes]
            H.ContSat (H.Satisfies x y)  -> do
              contP   <- F.appifyF a <$> cTrans (H.Var $ recVar x) (H.substsC (zip (map (H.Var . recVar) fs) fs) y)
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
