module Translation where

import qualified Haskell as H
import qualified FOL as F
import Control.Monad.State
import Data.Char (toUpper)
import Data.Maybe (fromJust)

-- Type signatures:
-- eTrans :: H.Expression -> F.Term
-- dTrans :: H.Definition -> F.Formula
-- sTrans :: H.Expression -> H.Contract -> Fresh F.Formula
-- tTrans :: H.DataType -> Fresh [F.Formula]
-- trans  :: H.DefGeneral -> [F.Formula]

type Fresh = State (String,Int)

-- Expression
------------

eTrans :: H.Expression -> F.Term
eTrans (H.Var v) = F.Var v
eTrans (H.Fun f) = F.Fun f
eTrans (H.App e1 e2) = F.App (eTrans e1) (eTrans e2)
eTrans H.BAD = F.BAD
eTrans (H.Con d) = F.Var d

-- A little helper function for later
eTransfxi f vs = eTrans $ H.apps (H.Fun f:map H.Var vs)



-- Definition
-------------

dTrans :: H.Definition -> F.Formula
dTrans (H.Let f vs e) = F.foralls vs $ (eTransfxi f vs) `F.Eq` (eTrans e)
dTrans (H.LetCase f vs e pes) = 
  F.foralls vs' $ (F.foralls zs $ (foldl (\fo (pi,ei)-> F.And fo $ ((eTrans e) `F.Eq` (eTrans (H.apps $ H.Var (head pi) : map H.Var (take (fromJust $ lookup (head pi) context) zs)))) `F.Implies` 
                                                                        ((eTransfxi f vs') `F.Eq` eTrans ei)) F.True pes)) `F.And` 
  ((eTrans e `F.Eq` F.BAD) `F.Implies` (eTransfxi f vs' `F.Eq` F.BAD)) `F.And`  -- Eq 10
  (((F.Not $ eTrans e `F.Eq` F.BAD) `F.And` (foldl (\f (d,a) -> f `F.And` (F.Not $ F.Eq (eTrans e) $ F.apps (F.Var d:(sels d a)))) F.True context)) `F.Implies` -- Eq 11
   (eTransfxi f vs' `F.Eq` F.UNR)) -- Eq 12
    where context = map (\p -> (head p, length $ tail p)) $ map fst pes :: [(String,Int)]
          sels d a = [(F.Var $ "sel_"++(show i)++"_"++d) `F.App` eTrans e | i <- [1..a]]
          zs = ["Zz"++(show x) | x <- [1..(foldl1 max [snd y | y <- context])]]
          vs' = map (map toUpper) vs



-- Contract satisfaction
------------------------

sTrans :: H.Expression -> H.Contract -> Fresh F.Formula
sTrans e H.Any = return F.True

sTrans e (H.Pred x u) = return $ (eTrans e `F.Eq` F.UNR) `F.Or` ((F.CF $ eTrans e) `F.And` 
                                                                  (F.CF $ eTrans u') `F.And` (F.Not $ eTrans u' `F.Eq` (F.Var "false")))
  where u' = H.subst u e x

sTrans e (H.AppC x c1 c2) = do
  (s,k) <- get
  put (s,k+1)
  let freshX = s++(show k) 
      c2' = H.substC c2 (H.Var freshX) x
  f1 <- sTrans (H.Var freshX) c1
  f2 <- sTrans (H.App e (H.Var freshX)) c2'
  return $ F.Forall freshX (f1 `F.Implies` f2)






-- Data constructors
--------------------

tTrans :: H.DataType -> Fresh [F.Formula]
tTrans d = liftM3 (+++) (s1 d) (s2 d) (s3 d)
  where (+++) a b c = a ++ b ++ c

s1 :: H.DataType -> Fresh [F.Formula]
s1 (H.Data _ dns) = sequence $ map s1D dns

-- It's the set S1 but for only one data constructor
s1D :: (String,Int) -> Fresh F.Formula
s1D (d,a) = do
  (s,k) <- get
  put (s,k+1)
  let xs = map (\n -> s++"_"++(show n)) [1..a]
  return $ F.foralls xs $ foldl (\f (x,k) -> f `F.And` (F.Eq (F.Var x) $ F.apps $ F.Var ("sel_"++(show k)++"_"++d) : F.Var d : map F.Var xs)) F.True (zip xs [1..a])


s2 :: H.DataType -> Fresh [F.Formula]
s2 (H.Data _ dns) = sequence $ map s2D [(a,b) | a <- dns, b <- dns, a < b]

-- It's S2 for a pair of data constructors.
s2D :: ((String,Int),(String,Int)) -> Fresh F.Formula
s2D ((d1,a1),(d2,a2)) = do
  (s,k) <- get
  put (s,k+2)
  let xs1 = map (\n -> s++(show k)++"_"++(show n)) [1..a1]
      xs2 = map (\n -> s++(show $ k + 1)++"_"++(show n)) [1..a2]
  return $ F.foralls xs1 $ F.foralls xs2 $ F.Not $ F.Eq (F.apps $ F.Var d1 : map F.Var xs1) (F.apps $ F.Var d2 : map F.Var xs2)


s3 :: H.DataType -> Fresh [F.Formula]
s3 (H.Data _ dns) = sequence $ map s3D dns

-- It's S3 but only for one data constructor
s3D :: (String,Int) -> Fresh F.Formula
s3D (d,a) = do
  (s,k) <- get
  put (s,k+1)
  let xs = map (\n -> s++(show k)++"_"++(show n)) [1..a]
  return $ F.foralls xs $ F.Iff (F.CF $ F.apps $ F.Var d : map F.Var xs) (foldl (\f x -> f `F.And` F.CF (F.Var x)) F.True xs)




-- Final translation
--------------------

trans  :: H.DefGeneral -> [F.Formula]
trans (H.Def d) = [dTrans d]
trans (H.DataType t) = evalState (tTrans t) ("A",0)
trans (H.ContSat (H.Satisfies v c)) = map F.Not [evalState (sTrans (H.Var v) c) ("Z",0)]


okFromd :: H.Definition -> H.Contract
okFromd (H.Let _ vs _) = foldl (\c _ -> H.AppC "dummy" c H.ok) H.ok vs

