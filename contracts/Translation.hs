module Translation where

import qualified Haskell as H
import qualified FOL as F
import Control.Monad.State
import Data.Char (toUpper)
import Data.Maybe (fromJust)
import Data.List (sort)
-- Type signatures:
-- eTrans :: H.Expression -> F.Term
-- dTrans :: H.Definition -> F.Formula
-- sTrans :: H.Expression -> H.Contract -> Fresh F.Formula
-- tTrans :: H.DataType -> Fresh [F.Formula]
-- trans  :: H.DefGeneral -> [F.Formula]

type Fresh = State (String,Int)



-- Expression
-------------

eTrans :: H.Expression -> F.Term F.Variable
eTrans (H.Var v) = F.Var $ F.Regular v
eTrans (H.Fun f) = F.Var $ F.Regular f
eTrans (H.App e1 e2) = F.App [eTrans e1,eTrans e2] -- TODO modifier H.App
eTrans H.BAD = F.Var $ F.BAD
eTrans (H.Con d) = F.Var $ F.Regular d

-- A little helper function for later
eTransfxi f vs = eTrans $ H.apps (H.Fun f:map H.Var vs)



-- Definition
-------------

dTrans :: H.Definition -> F.Formula (F.Term F.Variable)
dTrans (H.Let f vs e) = F.Forall vvs $ F.Eq (F.App (eTrans (H.Var f):vvs)) (eTrans e)
  where vvs = map (F.Var . F.Regular) vs
dTrans (H.LetCase f vs e pes) = 
  F.Forall (vvs ++ zs) $ F.And (eq9++[eq10,eq11])
  where vvs = map (F.Var . F.Regular) vs
        arities = map (\p -> (head p, length $ tail p)) $ map fst pes :: [(String,Int)]
        zs = [F.Var $ F.Regular $ "Zdef" ++ show x | x <- [1..(foldl1 max [snd y | y <- arities])]]
        eq9 = [(eTrans e `F.Eq` (F.App ((F.Var $ F.Regular $ head pi):(take (length pi - 1) [ z | (v,z) <- zip (tail pi) zs ])))) `F.Implies` (F.App (eTrans (H.Var f):vvs) `F.Eq` (eTrans (zedify ei pi))) | (pi,ei) <- pes]
        eq10 = (eTrans e `F.Eq` (F.Var F.BAD)) `F.Implies` F.Not (F.App (eTrans (H.Var f):vvs) `F.Eq` F.Var F.BAD)
        eq11 = (F.And $ (F.Not $ eTrans e `F.Eq` F.Var F.BAD):bigAndSel ) `F.Implies` eq12
        eq12 = (F.App (eTrans (H.Var f):vvs) `F.Eq` F.Var F.UNR)
        bigAndSel = [F.Not $ eTrans e `F.Eq` F.App ((F.Var (F.Regular di)):[F.App [(F.Var . F.Regular) ("sel_"++(show i)++"_"++di),eTrans e] | i <- [1..ai]]) | (di,ai) <- arities]
        zedify ei pi = foldl (\e (v,z) -> H.subst e (H.Var $ extractVR z) (v)) ei (take (length (tail pi)) $ zip (tail pi) zs)
        extractVR (F.Var (F.Regular v)) = v

test = (H.LetCase "head" ["xyz"] (H.Var "xyz") [(["nil"],H.BAD),(["cons","a","b"],H.Var "a")])
-- t = putStrLn $ (trans test) >>= (F.simplify) >>= F.toTPTP


-- Contract satisfaction
------------------------

sTrans :: H.Expression -> H.Contract -> Fresh (F.Formula (F.Term F.Variable))
sTrans e H.Any = return F.True

sTrans e (H.Pred x u) = return $ F.Or [(eTrans e `F.Eq` F.Var F.UNR) ,F.And [F.CF $ eTrans e ,             (F.Not $ F.Eq (F.Var F.BAD) $ eTrans u') , F.Not $ eTrans u' `F.Eq` (F.Var $ F.Regular "false")]] -- The data constructor False.
  where u' = H.subst u e x

sTrans e (H.AppC x c1 c2) = do
  (s,k) <- get
  put (s,k+1)
  let freshX = s++(show k) 
      c2' = H.substC c2 (H.Var freshX) x
  f1 <- sTrans (H.Var freshX) c1
  f2 <- sTrans (H.App e (H.Var freshX)) c2'
  return $ F.Forall [F.Var $ F.Regular $ freshX] (f1 `F.Implies` f2)






-- Data constructors
--------------------

tTrans :: H.DataType -> Fresh [F.Formula (F.Term F.Variable)]
tTrans d = liftM4 (++++) (s1 d) (s2 d) (s3 d) (return $ s4 d)
  where (++++) a b c d = a ++ b ++ c ++ d

s1 :: H.DataType -> Fresh [F.Formula (F.Term F.Variable)]
s1 (H.Data _ dns) = sequence $ map s1D dns

-- It's the set S1 but for only one data constructor
s1D :: (String,Int) -> Fresh (F.Formula (F.Term F.Variable))
s1D (d,a) = do
  (s,k) <- get
  put (s,k+1)
  let xs = map (\n -> s++"_"++(show n)) [1..a]
  return $ F.Forall (map (F.Var . F.Regular) xs) $ F.And [F.Eq (F.Var $ F.Regular x) $ F.App [(F.Var $ F.Regular ("sel_"++(show k)++"_"++d)) , F.App $ (F.Var $ F.Regular d) : map (F.Var . F.Regular) xs] | (x,k) <- zip xs [1..a]]


s2 :: H.DataType -> Fresh [F.Formula (F.Term F.Variable)]
s2 (H.Data _ dns) = sequence $ map s2D [(a,b) | a <- dns, b <- dns, a < b]

-- It's S2 for a pair of data constructors.
s2D :: ((String,Int),(String,Int)) -> Fresh (F.Formula (F.Term F.Variable))
s2D ((d1,a1),(d2,a2)) = do
  (s,k) <- get
  put (s,k+2)
  let xs1 = map (\n -> s++(show k)++"_"++(show n)) [1..a1]
      xs2 = map (\n -> s++(show $ k + 1)++"_"++(show n)) [1..a2]
  return $ F.Forall (map (F.Var . F.Regular) (xs1 ++ xs2)) $ F.Not $ F.Eq (F.App $ (F.Var . F.Regular) d1 : map (F.Var . F.Regular) xs1) (F.App $ (F.Var . F.Regular) d2 : map (F.Var . F.Regular) xs2)


s3 :: H.DataType -> Fresh [F.Formula (F.Term F.Variable)]
s3 (H.Data _ dns) = sequence $ map s3D dns

-- It's S3 but only for one data constructor
s3D :: (String,Int) -> Fresh (F.Formula (F.Term F.Variable))
s3D (d,a) = do
  (s,k) <- get
  put (s,k+1)
  let xs = map (\n -> s++(show k)++"_"++(show n)) [1..a]
  if xs /= [] 
    then (return $ F.Forall (map (F.Var . F.Regular) xs) $ F.Iff (F.CF $ F.App $ (F.Var . F.Regular) d : map (F.Var . F.Regular) xs) (F.And [F.CF (F.Var $ F.Regular x) | x <- xs]))
    else return $ F.CF $ F.App [F.Var $ F.Regular d]

s4 :: H.DataType -> [F.Formula (F.Term F.Variable)]
s4 (H.Data _ dns) = map (\(d,a) -> F.Not ((F.Var . F.Regular) d `F.Eq` F.Var F.UNR)) dns




-- Final translation
--------------------

trans :: H.DefGeneral -> [F.Formula (F.Term F.Variable)]
trans = undefined
-- trans (H.Def d) = [dTrans d]
-- trans (H.DataType t) = evalState (tTrans t) ("Dtype",0)
-- trans (H.ContSat (H.Satisfies v c)) = map F.Not [evalState (sTrans (H.Var v') c') ("Zcont",0)] ++ [(F.Forall "F" $ F.Forall "X" $ (F.And (F.CF $ F.Var "X") (F.CF $ F.Var "F")) `F.Implies` (F.CF $ (F.App (F.Var "F") (F.Var "X")))),F.Not $ F.CF F.BAD,F.CF F.UNR]
--   where v' = v
--         c' = H.subst 

transExp = aux . sort

aux (H.ContSat (H.Satisfies v c):ds) = map F.Not [evalState (sTrans (H.Var v) c) ("Z",0)] ++ [evalState (sTrans (H.Var v') c) ("Zp",0)] ++ concatMap treat ds ++ footer
  where treat (H.DataType t) = evalState (tTrans t) ("D",0)
        treat (H.Def d@(H.Let x xs e)) = [if x == v then dTrans $ H.Let x xs (H.subst e (H.Var v') x) else dTrans d]
        treat (H.Def d@(H.LetCase x xs e pes)) = [if x == v then dTrans $ H.LetCase x xs (H.subst e (H.Var v') x) (map (\(p,e) -> (p,H.subst e (H.Var v') x)) pes) else dTrans d]
        v' = v++"p"
        footer = [(F.Forall (map (F.Var . F.Regular) ["F","X"]) $ (F.And [F.CF $ F.Var $ F.Regular "X", F.CF $ F.Var $ F.Regular "F"]) `F.Implies` (F.CF $ (F.App [(F.Var $ F.Regular "F"), (F.Var $ F.Regular "X")]))),F.Not $ F.CF $ F.Var $ F.BAD,F.CF $ F.Var $ F.UNR]
aux _ = undefined


-- okFromd :: H.Definition -> H.Contract
-- okFromd (H.Let _ vs _) = foldl (\c _ -> H.AppC "dummy" c H.ok) H.ok vs
