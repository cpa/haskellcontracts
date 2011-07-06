module Translation where

import Debug.Trace
import qualified Haskell as H
import qualified FOL as F
import Control.Monad.State
import Data.Char (toUpper)
import Data.Maybe (fromJust)
import Data.List (sort,partition)

type Fresh = State TransState

data TransState = S { prefix  :: String
                    , count   :: Int
                    , fofBag  :: [F.Formula (F.Term F.Variable)]
                    , arities :: [(String,Int)]}



-- Expression
-------------

--eTrans :: H.Expression -> Fresh (F.Term F.Variable)
eTrans (H.Var v) = return $ (F.Var $ F.Regular v)
eTrans (H.App e1 e2) = do 
  t1 <- eTrans e1
  t2 <- eTrans e2 
  return $ F.App [t1,t2] -- TODO modifier H.App
eTrans (H.FullApp f es) = do
  ts <- sequence $ map eTrans es
  return $ F.FullApp (F.Regular f) ts
eTrans H.BAD = return $ F.Var $ F.BAD
eTrans (H.Sat e c) = do 
  ts <- sTrans (H.Var "x") c
  te <- eTrans $ H.FullApp "satC" [H.Var "x"]
  let fe = F.Forall [F.Var $ F.Regular "x"] $ F.Iff ts $ F.Eq (F.Var $ F.Regular "true") te
  modify (\s -> s {fofBag = fe:(fofBag s)})
  eTrans e
eTrans (H.CF e) = do
  te <- eTrans e
  modify (\s -> s {fofBag = F.CF te:fofBag s})
  return $ F.Var $ F.Regular "true"




-- Definition
-------------

--dTrans :: H.Definition -> Fresh [F.Formula (F.Term F.Variable)]
dTrans (H.Let f vs e) = do
  et <- eTrans e
  return $ [F.Forall vvs $ F.Eq (F.FullApp (F.Regular f) vvs) (F.Weak $ et),fptr1,fptr2,fptr3]
  where vvs = map (F.Var . F.Regular) vs
        fptr1 = F.Iff (F.Forall vvs $ F.Implies (F.And [F.CF v | v <- vvs]) $ F.CF (F.FullApp (F.Regular f) vvs)) (F.CF $ F.Var $ F.Regular (f++"_ptr"))
        fptr2 = F.Forall vvs $ F.Eq (F.FullApp (F.Regular f) vvs) (F.App $ (F.Var . F.Regular) (f++"_ptr") : vvs)
        fptr3 = F.Forall vvs $ F.Eq (F.FullApp (F.Regular (f ++ "p")) vvs) (F.App $ (F.Var . F.Regular) (f++"p_ptr") : vvs)

dTrans (H.LetCase f vs e pes) = do
  et <- eTrans e
  ft <- eTrans $ H.Var f
  let zedify ei pi = foldl (\e (v,z) -> H.subst e (H.Var $ extractVR z) (v)) ei (take (length (tail pi)) $ zip (tail pi) zs)
      extractVR (F.Var (F.Regular v)) = v 
      arities = map (\p -> (head p, length $ tail p)) $ map fst pes :: [(String,Int)]
      zs = [F.Var $ F.Regular $ "Zdef" ++ show x | x <- [1..(foldl1 max [snd y | y <- arities])]]
  tpieis <- sequence [eTrans (zedify ei pi) | (pi,ei) <- pes]
  let vvs = map (F.Var . F.Regular) vs
      eq9 = [(et `F.Eq` (F.FullApp (F.Regular $ head pi) (take (length pi - 1) [ z | (v,z) <- zip (tail pi) zs ]))) `F.Implies` (F.FullApp (F.Regular f) vvs `F.Eq` (F.Weak $ tpiei)) | ((pi,ei),tpiei) <- zip pes tpieis]
      eq10 = (et `F.Eq` (F.Var F.BAD)) `F.Implies` (F.FullApp (F.Regular f) vvs `F.Eq` F.Var F.BAD)
      eq11 = (F.And $ (F.Not $ et `F.Eq` F.Var F.BAD):bigAndSel ) `F.Implies` eq12
      eq12 = (F.FullApp (F.Regular f) vvs `F.Eq` F.Var F.UNR)
      bigAndSel = [F.Not $ et `F.Eq` F.Weak (F.FullApp (F.Regular di) [F.FullApp (F.Regular ("sel_"++(show i)++"_"++di)) [et] | i <- [1..ai]]) | (di,ai) <- arities]
      fptr1 = F.Iff (F.Forall vvs $ F.Implies (F.And [F.CF v | v <- vvs]) $ F.CF (F.FullApp (F.Regular f) vvs)) (F.CF $ F.Var $ F.Regular (f++"_ptr"))
      fptr2 = F.Forall vvs $ F.Eq (F.FullApp (F.Regular f) vvs) (F.App $ (F.Var . F.Regular) (f++"_ptr") : vvs)
      fptr3 = F.Forall vvs $ F.Eq (F.FullApp (F.Regular (f ++ "p")) vvs) (F.App $ (F.Var . F.Regular) (f++"p_ptr") : vvs)

  return $ [F.Forall (vvs ++ zs) $ F.And (eq9++[eq10,eq11]),fptr1,fptr2,fptr3]

test = (H.LetCase "head" ["xyz"] (H.Var "xyz") [(["nil"],H.BAD),(["cons","a","b"],H.Var "a")])
-- t = putStrLn $ (trans test) >>= (F.simplify) >>= F.toTPTP


-- Contract satisfaction
------------------------

--sTrans :: H.Expression -> H.Contract -> Fresh (F.Formula (F.Term F.Variable))
sTrans e H.Any = return F.True

sTrans e (H.Pred x u) =  do
  et <- eTrans e
  ut' <- eTrans u'
  s <- get
  -- (a,b,fs) <- get
  put $ s {fofBag = []}
  return $ F.And $ [F.Or [(et `F.Eq` F.Var F.UNR) ,F.And [(F.Not $ F.Eq (F.Var F.BAD) $ ut') , F.Not $ ut' `F.Eq` (F.Var $ F.Regular "false")]]]++(fofBag s) -- The data constructor False.
  where u' = H.subst u e x

sTrans e (H.AppC x c1 c2) = do
  S s k fs a <- get
  put $ S s (k+1) fs a
  let freshX = s++(show k) 
      c2' = H.substC c2 (H.Var freshX) x
  f1 <- sTrans (H.Var freshX) c1
  f2 <- case e of 
    H.Var x -> sTrans (H.appifyExpr [("f",3)] $  H.apps (H.Var x:[H.Var $ freshX])) c2'
--    H.FullApp x xs -> sTrans (H.apps $ ((H.Var x:xs)++[H.Var $ freshX])) c2' -- TODO WRONG
    _ -> sTrans (H.App e (H.Var freshX)) c2'
  return $ F.Forall [F.Var $ F.Regular $ freshX] (f1 `F.Implies` f2)

sTrans e (H.And c1 c2) = do
  f1 <- sTrans e c1
  f2 <- sTrans e c2
  return $ F.And [f1,f2]
  
sTrans e (H.Or c1 c2) = do
  f1 <- sTrans e c1
  f2 <- sTrans e c2
  return $ F.Or [f1,f2]



-- Data constructors
--------------------

--tTrans :: H.DataType -> Fresh [F.Formula (F.Term F.Variable)]
tTrans d = liftM5 (+++++) (s1 d) (s2 d) (s3 d) (s4 d) (s5 d)
  where (+++++) a b c d e = a ++ b ++ c ++ d ++ e

--s1 :: H.DataType -> Fresh [F.Formula (F.Term F.Variable)]
s1 (H.Data _ dns) = sequence $ map s1D dns

-- It's the set S1 but for only one data constructor
--s1D :: (String,Int,H.Contract) -> Fresh (F.Formula (F.Term F.Variable))
s1D (d,a,c) = do
  s <- get
  let k = count s 
  put $ s {count = k+1}
  let xs = map (\n -> (prefix s)++"_"++(show n)) [1..a]
  return $ F.Forall (map (F.Var . F.Regular) xs) $ F.And [F.Eq (F.Var $ F.Regular x) $ F.FullApp (F.Regular ("sel_"++(show k)++"_"++d)) [(F.FullApp (F.Regular d) $ map (F.Var . F.Regular) xs)] | (x,k) <- zip xs [1..a]]


--s2 :: H.DataType -> Fresh [F.Formula (F.Term F.Variable)]
s2 (H.Data _ dns) = sequence $ map s2D [(a,b) | a <- dns, b <- dns, a < b]

-- It's S2 for a pair of data constructors.
--s2D :: ((String,Int,H.Contract),(String,Int,H.Contract)) -> Fresh (F.Formula (F.Term F.Variable))
s2D ((d1,a1,c1),(d2,a2,c2)) = do
  s <- get
  let k = count s
  put $ s { count = k+2 }
  let xs1 = map (\n -> (prefix s)++(show k)++"_"++(show n)) [1..a1]
      xs2 = map (\n -> (prefix s)++(show $ k + 1)++"_"++(show n)) [1..a2]
  return $ F.Forall (map (F.Var . F.Regular) (xs1 ++ xs2)) $ F.Not $ F.Eq (F.FullApp (F.Regular d1) (map (F.Var . F.Regular) xs1)) (F.FullApp (F.Regular d2) (map (F.Var . F.Regular) xs2))


--s3 :: H.DataType -> Fresh [F.Formula (F.Term F.Variable)]
s3 (H.Data _ dns) = sequence $ map s3D dns

--- It's S3 but only for one data constructor
--s3D :: (String,Int,H.Contract) -> Fresh (F.Formula (F.Term F.Variable))
s3D (d,a,c) = do
  s <- get
  let k = count s
  put $ s { count = k+1 }
  let xs = map (\n -> (prefix s)++(show k)++"_"++(show n)) [1..a]
  if xs /= []
    then (return $ F.Forall (map (F.Var . F.Regular) xs) $ F.Iff (F.CF $ F.FullApp (F.Regular d) (map (F.Var . F.Regular) xs)) (F.And [F.CF (F.Var $ F.Regular x) | x <- xs]))
    else return $ F.CF $ F.App [F.Var $ F.Regular d]

--s4 :: H.DataType -> Fresh [F.Formula (F.Term F.Variable)]
s4 (H.Data _ dns) = sequence $ map s4D dns

--s4D :: (String,Int,H.Contract) -> Fresh (F.Formula (F.Term F.Variable))
s4D (d,a,c) = do
  s <- get
  let k = count s
  put $ s { count = k+1 }
  let xs = map (\n -> (prefix s)++(show k)++"_"++(show n)) [1..a]
  et <- eTrans $ H.FullApp d (map H.Var xs)
  if xs /= [] 
    then (return $ F.Forall (map (F.Var . F.Regular) xs) $ F.Not (et `F.Eq` (F.Var F.UNR)))
    else return $ F.Not ((F.Var $ F.Regular d) `F.Eq` F.Var F.UNR)

s5 _ = return []
-- s5 :: H.DataType -> Fresh [F.Formula (F.Term F.Variable)]
-- s5 (H.Data _ dns) = sequence $ map s5D dns

-- s5D :: (String,Int,H.Contract) -> Fresh (F.Formula (F.Term F.Variable))
-- s5D (d,a,c) = do
--   (s,k,fs) <- get
--   put (s,k+1,fs)
--   let xs = map (\n -> s++(show k)++"_"++(show n)) [1..a]
--       cs = H.toList c
--       dapp = H.FullApp d [H.Var x | x <- xs]
--   sxs <- sequence $ [sTrans (H.Var xi) ci | (xi,ci) <- zip xs cs]
--   st <- sTrans dapp c
--   if xs /= [] then return $ F.Forall (map (F.Var . F.Regular) xs) $ F.Iff st (F.And sxs)
--     else sTrans (H.Var d) c







-- Final translation
--------------------

--trans :: [H.DefGeneral] -> String -> [F.Formula (F.Term F.Variable)]
trans ds fcheck = aux fcheck ds

isContToCheck fcheck (H.ContSat (H.Satisfies v c)) = v==fcheck
isContToCheck _ _ = False


aux fcheck ds = map F.Not [evalState (sTrans (H.Var v) c) (S "Z" 0 [] a)] ++ [evalState (sTrans (H.Var v') c) (S "Zp" 0 [] a)] ++ concatMap treat ds' ++ footer
  where ([H.ContSat (H.Satisfies v c)],ds') = partition (isContToCheck fcheck) ds
        treat (H.DataType t) = evalState (tTrans t) (S "P" 0 [] a)
        treat (H.Def d@(H.Let x xs e)) = if x == v then evalState (dTrans $ H.Let x xs (H.subst e (H.Var v') x)) (S "O" 0 [] a) else evalState (dTrans d) $ S "P" 0 [] a
        treat (H.Def d@(H.LetCase x xs e pes)) = if x == v then evalState (dTrans $ H.LetCase x xs (H.subst e (H.Var v') x) (map (\(p,e) -> (p,H.subst e (H.Var v') x)) pes)) (S "O" 0 [] a) else evalState (dTrans d) (S "P" 0 [] a)
        treat (H.ContSat (H.Satisfies x y)) = [evalState (sTrans (H.Var x) y) (S "Y" 0 [] a)]
        v' = v++"p"
        footer = [(F.Forall (map (F.Var . F.Regular) ["F","X"]) $ (F.And [F.CF $ F.Var $ F.Regular "X", F.CF $ F.Var $ F.Regular "F"]) `F.Implies` (F.CF $ (F.App [(F.Var $ F.Regular "F"), (F.Var $ F.Regular "X")]))),F.Not $ F.CF $ F.Var $ F.BAD,F.CF $ F.Var $ F.UNR]
        a = H.arities ds