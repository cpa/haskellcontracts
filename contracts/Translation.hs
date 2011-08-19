module Translation where

import qualified Haskell as H
import qualified FOL as F
import FOL (MetaFormula(..))
import Control.Monad.State
import Data.List (partition)
import Control.Applicative

type Fresh = State TransState

data TransState = S { prefix  :: String -- the prefix of our fresh variables
                    , count   :: Int    -- a counter for the suffix of our fresh variables
                    , arities :: [H.Type H.Variable]} -- The arities of functions/data constructors in the program


-- Expression
-------------

eTrans (H.Var v) = return $ (F.Var $ F.Regular v)
eTrans (H.App e1 e2) = do 
  t1 <- eTrans e1
  t2 <- eTrans e2 
  return $ F.App [t1,t2] 
eTrans (H.FullApp f es) = do
  ts <- sequence $ map eTrans es
  return $ F.FullApp (F.Regular f) ts
eTrans H.BAD = return $ F.Var F.BAD



-- Definition
-------------

dTrans (H.Let f vs e) = do
  et <- eTrans e
  s <- get
  let freshT = F.Var $ F.Regular $ prefix s ++ (show $ count s)
  return $ [F.Forall (freshT:vvs) $ F.And [F.Min freshT, freshT :=: F.FullApp (F.Regular f) vvs ] :=>: freshT :=: (F.Weak $ et),fptr1,fptr2,fptr3]
  where vvs = map (F.Var . F.Regular) vs
        fptr1 = (F.Forall vvs $ (F.And [F.CF v | v <- vvs]) :=>: F.CF (F.FullApp (F.Regular f) vvs)) :<=>: (F.CF $ F.Var $ F.Regular (f++"_ptr"))
        fptr2 = F.Forall vvs $ (F.FullApp (F.Regular f) vvs) :=: (F.Weak (F.App $ (F.Var . F.Regular) (f++"_ptr") : vvs))
        fptr3 = F.Forall vvs $ (F.FullApp (F.Regular (f ++ "p")) vvs) :=: (F.Weak (F.App $ (F.Var . F.Regular) (f++"p_ptr") : vvs))

dTrans (H.LetCase f vs e pes) = do
  et <- eTrans e
  ft <- eTrans $ H.Var f
  s <- get
  let freshT = F.Var $ F.Regular $ prefix s ++ (show $ count s)
  let zedify ei pi = foldl (\e (v,z) -> H.subst (H.Var $ extractVR z) v e) ei (take (length (tail pi)) $ zip (tail pi) zs)
      extractVR (F.Var (F.Regular v)) = v 
      arities = map (\p -> (head p, length $ tail p)) $ map fst pes :: [(String,Int)]
      zs = [F.Var $ F.Regular $ "Zdef" ++ show x | x <- [1..(foldl1 max [snd y | y <- arities])]]
  tpieis <- sequence [eTrans (zedify ei pi) | (pi,ei) <- pes]
  let vvs = map (F.Var . F.Regular) vs
      eq1 = [F.And [F.Min freshT, et :=: F.FullApp (F.Regular $ head pi) (take (length pi - 1) [ z | (v,z) <- zip (tail pi) zs ]), freshT :=: F.FullApp (F.Regular f) vvs] :=>: (freshT :=: F.Var F.BAD) | (pi,_) <- pes]
--      eq1 = [(et :=: (F.FullApp (F.Regular $ head pi) (take (length pi - 1) [ z | (v,z) <- zip (tail pi) zs ]))) :=>: (F.FullApp (F.Regular f) vvs :=: (F.Weak $ tpiei)) | ((pi,ei),tpiei) <- zip pes tpieis]
      eq2 = F.And [F.Min freshT, (et :=: (F.Var F.BAD)), freshT :=: F.FullApp (F.Regular f) vvs] :=>: (freshT :=: F.Var F.BAD)
      eq3 = F.And [F.Min freshT, freshT :=: F.FullApp (F.Regular f) vvs] :=>: F.And [F.Min et, F.Or $ [et :=: F.Var F.BAD, freshT :=: F.Var F.UNR] ++ 
                                                                                                      [et :=: F.Weak (F.FullApp (F.Regular $ head pi) (take (length pi - 1) [ z | (v,z) <- zip (tail pi) zs ])) | (pi,ei) <- pes ]]

--      eq3 = (F.And $ (et :/=: F.Var F.BAD):bigAndSel ) :=>: eq4
      eq4 = (F.FullApp (F.Regular f) vvs :=: F.Var F.UNR)
--      bigAndSel = [et :/=: F.Weak (F.FullApp (F.Regular di) [F.FullApp (F.Regular ("sel_"++(show i)++"_"++di)) [et] | i <- [1..ai]]) | (di,ai) <- arities]
      fptr1 = (F.Forall vvs $ (F.And [F.CF v | v <- vvs]) :=>: F.CF (F.FullApp (F.Regular f) vvs)) :<=>: (F.CF $ F.Var $ F.Regular (f++"_ptr"))
      fptr2 = F.Forall vvs $ (F.FullApp (F.Regular f) vvs) :=: (F.Weak (F.App $ (F.Var . F.Regular) (f++"_ptr") : vvs))
      fptr3 = F.Forall vvs $ (F.FullApp (F.Regular (f ++ "p")) vvs) :=: (F.Weak (F.App $ (F.Var . F.Regular) (f++"p_ptr") : vvs))

  return $ [F.Forall (freshT:vvs ++ zs) $ F.And (eq1++[eq2,eq3]),fptr1,fptr2,fptr3]




-- Contract satisfaction
------------------------


sTrans e (H.Pred x u) =  do
  let  u' = H.subst e x u
  ut' <- eTrans u'
  et <- eTrans e
  s <- get
  let a = prefix s
      b = count s
  return $ [F.And [F.Min ut',F.Or [ut' :=: F.Var (F.Regular "true"), et :=: F.Var F.UNR]]]

sTrans e (H.AppC x c1 c2) = do
  s <- get
  let k = count s
  put $ s {count = k + 1}
  let freshX = (prefix s)++(show $ k) 
      c2' = H.substC (H.Var freshX) x c2
  [f1] <- dsTrans (H.Var freshX) c1
  let ex = case e of 
        H.Var x -> H.apps $ H.Var x:[H.Var $ freshX]
        _ -> H.App e (H.Var freshX)
  f2 <- sTrans ex c2'
  ex' <- eTrans ex
  return $ [F.Forall [F.Var $ F.Regular $ freshX] (F.Min ex' :=>: (F.Or $ f1: f2))]

sTrans e (H.And c1 c2) = do
  [f1] <- sTrans e c1
  [f2] <- sTrans e c2
  return $ [F.And [f1,f2]]
  
sTrans e (H.Or c1 c2) = do
  [f1] <- sTrans e c1
  [f2] <- sTrans e c2
  return $ [F.Or [f1,f2]]

sTrans e (H.CF) = do
  et <- eTrans e
  return $ [F.CF $ et]
  
dsTrans e (H.Pred x u) =  do
  let  u' = H.subst e x u
  ut' <- eTrans u'
  et <- eTrans e
  s <- get
  let a = prefix s
      b = count s
  return $ [F.And [F.Min ut',F.Or [ut' :=: (F.Var $ F.Regular "false"), et :=: F.Var F.BAD]]]

dsTrans e (H.AppC x c1 c2) = do
  s <- get
  let k = count s
  put $ s {count = k + 1}
  let freshX = (prefix s)++(show $ k) 
      c2' = H.substC (H.Var freshX) x c2
  [f1] <- sTrans (H.Var freshX) c1
  let ex = case e of 
        H.Var x -> H.apps $ H.Var x:[H.Var $ freshX]
        _ -> H.App e (H.Var freshX)
  f2 <- dsTrans ex c2'
  ex' <- eTrans ex
  return $ [F.Not $ F.Forall [F.Var $ F.Regular $ freshX] $ F.Not (F.And $ f1:f2)]

dsTrans e (H.And c1 c2) = do
  [f1] <- dsTrans e c1
  [f2] <- dsTrans e c2
  return $ [F.Or [f1,f2]]

dsTrans e (H.Or c1 c2) = do
  [f1] <- dsTrans e c1
  [f2] <- dsTrans e c2
  return $ [F.And [f1,f2]]

dsTrans e (H.CF) = do
  et <- eTrans e
  return $ [F.Not $ F.CF et]

-- Data constructors
-----------------------

tTrans d = concat <$> (sequence [s1 d,s2 d,s3 d,s4 d])

--s1 :: H.DataType -> Fresh [F.Formula (F.Term F.Variable)]
s1 (H.Data _ dns) = sequence $ map s1D dns

-- It's the set S1 but for only one data constructor
--s1D :: (String,Int,H.Contract) -> Fresh (F.Formula (F.Term F.Variable))
s1D (d,a,c) = do
  s <- get
  let k = count s 
  put $ s {count = k+1}
  let freshT = F.Var $ F.Regular $ prefix s ++ (show $ count s)
      xs = map (\n -> (prefix s)++"_"++(show n)) [1..a]
  return $ F.Forall (freshT:map (F.Var . F.Regular) xs) $ (F.And [F.Min freshT, freshT :=: F.FullApp (F.Regular ("sel_"++(show k)++"_"++d)) [(F.FullApp (F.Regular d) $ map (F.Var . F.Regular) xs)]]):=>: F.And [(F.Var $ F.Regular x) :=: freshT | (x,k) <- zip xs [1..a]]


--s2 :: H.DataType -> Fresh [F.Formula (F.Term F.Variable)]
s2 (H.Data _ dns) = sequence $ map s2D [(a,b) | a <- dns, b <- dns, a < b]

-- It's S2 for a pair of data constructors.
--s2D :: ((String,Int,H.Contract),(String,Int,H.Contract)) -> Fresh (F.Formula (F.Term F.Variable))
s2D ((d1,a1,c1),(d2,a2,c2)) = do
  s <- get
  let k = count s
      freshT = F.Var $ F.Regular $ prefix s ++ (show $ count s)
  put $ s { count = k+2 }
  let xs1 = map (\n -> (prefix s)++(show k)++"_"++(show n)) [1..a1]
      xs2 = map (\n -> (prefix s)++(show $ k + 1)++"_"++(show n)) [1..a2]
  return $ F.Forall (freshT:map (F.Var . F.Regular) (xs1 ++ xs2)) $ F.Not $ F.And [F.Min freshT, freshT :=: (F.FullApp (F.Regular d1) (map (F.Var . F.Regular) xs1)), freshT :=: (F.FullApp (F.Regular d2) (map (F.Var . F.Regular) xs2))]


--s3 :: H.DataType -> Fresh [F.Formula (F.Term F.Variable)]
s3 (H.Data _ dns) = sequence $ map s3D dns

--- It's S3 but only for one data constructor
--s3D :: (String,Int,H.Contract) -> Fresh (F.Formula (F.Term F.Variable))
s3D (d,a,c) = do
  s <- get
  let k = count s
      freshT = F.Var $ F.Regular $ prefix s ++ (show $ count s)
  put $ s { count = k+1 }
  let xs = map (\n -> (prefix s)++(show k)++"_"++(show n)) [1..a]
  if xs /= []
    then (return $ F.Forall (freshT:map (F.Var . F.Regular) xs) $ F.And [F.Min freshT, freshT :=: F.FullApp (F.Regular d) (map (F.Var . F.Regular) xs)] :=>: (F.CF freshT :<=>: (F.And [F.CF (F.Var $ F.Regular x) | x <- xs])))
    else return $ F.Min (F.Var $ F.Regular d) :=>: (F.CF $ F.App [F.Var $ F.Regular d])

--s4 :: H.DataType -> Fresh [F.Formula (F.Term F.Variable)]
s4 (H.Data _ dns) = sequence $ map s4D dns

--s4D :: (String,Int,H.Contract) -> Fresh (F.Formula (F.Term F.Variable))
s4D (d,a,c) = do
  s <- get
  let k = count s
      freshT = F.Var $ F.Regular $ prefix s ++ (show $ count s)
  put $ s { count = k+1 }
  let xs = map (\n -> (prefix s)++(show k)++"_"++(show n)) [1..a]
  et <- eTrans $ H.FullApp d (map H.Var xs)
  if xs /= [] 
    then return $ F.Forall (freshT:map (F.Var . F.Regular) xs) $ F.And [F.Min freshT, freshT :=: et] :=>: freshT :/=: F.Var F.UNR
    else return $ F.Min (F.Var $ F.Regular d) :=>: (F.Var $ F.Regular d) :/=: F.Var F.UNR





-- Final translation
--------------------

isToCheck fs (H.Def (H.Let f _ _))         = f `elem` fs
isToCheck fs (H.Def (H.LetCase f _ _ _))   = f `elem` fs
isToCheck fs (H.ContSat (H.Satisfies f _)) = f `elem` fs
isToCheck _ _                              = False

trans :: H.Program -> [H.Variable] -> [F.Formula]
trans ds fs = evalState (go fs ((H.appify) ds)) (S "Z" 0 (H.arities ds))
  where go fs ds = do 
          booleanPrelude <- tTrans $ H.Data "Bool" [("true",0,H.ok),("false",0,H.ok)]
          let prelude = [(F.Forall (map (F.Var . F.Regular) ["F","X"]) $ (F.And [F.CF $ F.Var $ F.Regular "X", F.CF $ F.Var $ F.Regular "F"]) :=>: (F.CF $ (F.App [(F.Var $ F.Regular "F"), (F.Var $ F.Regular "X")])))] ++ booleanPrelude
          let (toCheck,regDefs) = partition (isToCheck fs) ds
              recVar x = x ++ "p"
          a <- fmap arities get
          regFormulae <- forM regDefs $ \d -> case d of
            H.DataType t                -> tTrans t
            H.Def d                     -> dTrans d
            H.ContSat (H.Satisfies x y) -> F.appifyF a <$> sTrans (H.Var x) y
          speFormulae <- forM toCheck $ \d -> case d of
            H.DataType t                 -> error "No contracts for datatypes yet!"
            H.Def (H.Let f xs e)         -> dTrans $ H.Let f xs (H.substs (zip (map (H.Var . recVar) fs) fs) e)
            H.Def (H.LetCase f xs e pes) -> dTrans $ H.LetCase f xs (H.substs (zip (map (H.Var . recVar) fs) fs) e) [(p,(H.substs (zip (map (H.Var . recVar) fs) fs) e)) | (p,e) <- pes]
            H.ContSat (H.Satisfies x y)  -> do
              contP   <- F.appifyF a <$> sTrans (H.Var $ recVar x) (H.substsC (zip (map (H.Var . recVar) fs) fs) y)
              notCont <- map F.Not <$> F.appifyF a <$> sTrans (H.Var x) y
              return $ notCont ++ contP
          return $ concat $ prelude : regFormulae ++ speFormulae 
