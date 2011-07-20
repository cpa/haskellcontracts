module Translation where

import qualified Haskell as H
import qualified FOL as F
import FOL (Formula(..))
import Control.Monad.State
import Data.List (partition)

type Fresh = State TransState

data TransState = S { prefix  :: String
                    , count   :: Int
                    , arities :: [(String,Int)]}


-- Expression
-------------

eTrans (H.Var v) = return $ (F.Var $ F.Regular v)
eTrans (H.App e1 e2) = do 
  t1 <- eTrans e1
  t2 <- eTrans e2 
  return $ F.App [t1,t2] -- TODO modifier H.App
eTrans (H.FullApp f es) = do
  ts <- sequence $ map eTrans es
  return $ F.FullApp (F.Regular f) ts


-- Definition
-------------

dTrans (H.Let f vs e) = do
  et <- eTrans e
  return $ [F.Forall vvs $ (F.FullApp (F.Regular f) vvs) :=: (F.Weak $ et),fptr1,fptr2,fptr3]
  where vvs = map (F.Var . F.Regular) vs
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
      -- FIXME: eqN is not related to n-th equation in the pdf anymore
      eq9 = [(et :=: (F.FullApp (F.Regular $ head pi) (take (length pi - 1) [ z | (v,z) <- zip (tail pi) zs ]))) :=>: (F.FullApp (F.Regular f) vvs :=: (F.Weak $ tpiei)) | ((pi,ei),tpiei) <- zip pes tpieis]
      eq10 = (et :=: (F.Var F.BAD)) :=>: (F.FullApp (F.Regular f) vvs :=: F.Var F.BAD)
      eq11 = (F.And $ (et :/=: F.Var F.BAD):bigAndSel ) :=>: eq12
      eq12 = (F.FullApp (F.Regular f) vvs :=: F.Var F.UNR)
      bigAndSel = [et :/=: F.Weak (F.FullApp (F.Regular di) [F.FullApp (F.Regular ("sel_"++(show i)++"_"++di)) [et] | i <- [1..ai]]) | (di,ai) <- arities]
      fptr1 = (F.Forall vvs $ (F.And [F.CF v | v <- vvs]) :=>: F.CF (F.FullApp (F.Regular f) vvs)) :<=>: (F.CF $ F.Var $ F.Regular (f++"_ptr"))
      fptr2 = F.Forall vvs $ (F.FullApp (F.Regular f) vvs) :=: (F.App $ (F.Var . F.Regular) (f++"_ptr") : vvs)
      fptr3 = F.Forall vvs $ (F.FullApp (F.Regular (f ++ "p")) vvs) :=: (F.App $ (F.Var . F.Regular) (f++"p_ptr") : vvs)

  return $ [F.Forall (vvs ++ zs) $ F.And (eq9++[eq10,eq11]),fptr1,fptr2,fptr3]




-- Contract satisfaction
------------------------


sTrans e H.Any = return [Top]

sTrans e (H.Pred x u) =  do
  a <- liftM arities get
  let  u' = H.subst (fmap (\f -> if (take 4 $ (reverse f)) == "rtp_" then reverse $ drop 4 $ reverse f else f) $ H.appifyExpr (map (\(a,b) -> (a++"_ptr",b)) $ a) e) x u
  et' <- eTrans e 
  ut' <- eTrans u'
  et <- eTrans $ fmap (\f -> if (take 4 $ (reverse f)) == "rtp_" then reverse $ drop 4 $ reverse f else f) $ H.appifyExpr (map (\(a,b) -> (a++"_ptr",b)) $ a) e
  s <- get
  let a = prefix s
      b = count s
  return $ [F.And $ [F.Or [(et :=: F.Var F.UNR) ,F.And [F.Var F.BAD :/=: ut' , ut' :/=: (F.Var $ F.Regular "false")]]]] -- The data constructor False.

sTrans e (H.AppC x c1 c2) = do
  S s k a <- get
  put $ S s (k+1) a
  let freshX = s++(show k) 
      c2' = H.substC (H.Var freshX) x c2
  [f1] <- sTrans (H.Var freshX) c1
  [f2] <- case e of 
    H.Var x -> sTrans (H.appifyExpr a $ H.apps (H.Var x:[H.Var $ freshX])) c2'
--    H.FullApp x xs -> sTrans (H.apps $ ((H.Var x:xs)++[H.Var $ freshX])) c2' -- TODO WRONG
    _ -> sTrans (H.App (H.appifyExpr a e) (H.Var freshX)) c2'
  return $ [F.Forall [F.Var $ F.Regular $ freshX] (f1 :=>: f2)]

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
  return $ [F.CF et]

-- -- Data constructors
-----------------------


tTrans d = liftM4 (++++) (s1 d) (s2 d) (s3 d) (s4 d)
  where (++++) a b c d = a ++ b ++ c ++ d

--s1 :: H.DataType -> Fresh [F.Formula (F.Term F.Variable)]
s1 (H.Data _ dns) = sequence $ map s1D dns

-- It's the set S1 but for only one data constructor
--s1D :: (String,Int,H.Contract) -> Fresh (F.Formula (F.Term F.Variable))
s1D (d,a,c) = do
  s <- get
  let k = count s 
  put $ s {count = k+1}
  let xs = map (\n -> (prefix s)++"_"++(show n)) [1..a]
  return $ F.Forall (map (F.Var . F.Regular) xs) $ F.And [(F.Var $ F.Regular x) :=: F.FullApp (F.Regular ("sel_"++(show k)++"_"++d)) [(F.FullApp (F.Regular d) $ map (F.Var . F.Regular) xs)] | (x,k) <- zip xs [1..a]]


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
  return $ F.Forall (map (F.Var . F.Regular) (xs1 ++ xs2)) $ (F.FullApp (F.Regular d1) (map (F.Var . F.Regular) xs1)) :/=: (F.FullApp (F.Regular d2) (map (F.Var . F.Regular) xs2))


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
    then (return $ F.Forall (map (F.Var . F.Regular) xs) $ (F.CF $ F.FullApp (F.Regular d) (map (F.Var . F.Regular) xs)) :<=>: (F.And [F.CF (F.Var $ F.Regular x) | x <- xs]))
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
    then return $ F.Forall (map (F.Var . F.Regular) xs) $ et :/=: F.Var F.UNR
    else return $ (F.Var $ F.Regular d) :/=: F.Var F.UNR






-- Final translation
--------------------

isToCheck fs (H.Def (H.Let f _ _))         = f `elem` fs
isToCheck fs (H.Def (H.LetCase f _ _ _))   = f `elem` fs
isToCheck fs (H.ContSat (H.Satisfies f _)) = f `elem` fs
isToCheck _ _                              = False

trans ds fs = evalState (go fs ds) (S "Z" 0 (H.arities ds))
  where go fs ds = do 
          let (toCheck,regDefs) = partition (isToCheck fs) ds
              recVar x = x ++ "p"
          regFormulae <- forM regDefs $ \d -> case d of
            H.DataType t                -> tTrans t
            H.Def d                     -> dTrans d
            H.ContSat (H.Satisfies x y) -> sTrans (H.Var x) y
          speFormulae <- forM toCheck $ \d -> case d of
            H.DataType t                 -> error "No contracts for datatypes yet!"
            H.Def (H.Let f xs e)         -> dTrans $ H.Let f xs (H.substs (zip (map (H.Var . recVar) fs) fs) e)
            H.Def (H.LetCase f xs e pes) -> dTrans $ H.LetCase f xs (H.substs (zip (map (H.Var . recVar) fs) fs) e) [(p,(H.substs (zip (map (H.Var . recVar) fs) fs) e)) | (p,e) <- pes]
            H.ContSat (H.Satisfies x y)  -> do
              contP   <- sTrans (H.Var $ recVar x) $ H.substsC (zip (map (H.Var . recVar) fs) fs) y
              notCont <- liftM (map F.Not) $ sTrans (H.Var x) y
              return $ notCont ++ contP
          return $ concat $ header : regFormulae ++ speFormulae 
            where header = [(F.Forall (map (F.Var . F.Regular) ["F","X"]) $ (F.And [F.CF $ F.Var $ F.Regular "X", F.CF $ F.Var $ F.Regular "F"]) :=>: (F.CF $ (F.App [(F.Var $ F.Regular "F"), (F.Var $ F.Regular "X")])))
                           ,F.Not $ F.CF $ F.Var $ F.BAD,F.CF $ F.Var $ F.UNR,(F.Var $ F.Regular "false") :/=: (F.Var $ F.Regular "true")
                           ,F.CF (F.Var $ F.Regular "true"),F.CF (F.Var $ F.Regular "false"),(F.Var $ F.Regular "true") :/=: (F.Var $ F.Regular "unr"),(F.Var $ F.Regular "false") :/=: (F.Var $ F.Regular "unr")]
