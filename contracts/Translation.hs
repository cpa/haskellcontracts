module Translation where

import qualified Haskell as H
import qualified FOL as F
import Data.List (foldl')
import Control.Monad.State

import System.IO.Unsafe

eTrans :: H.Expression -> F.Term
dTrans :: H.Definition -> F.Formula
sTrans :: H.Expression -> H.Contract -> Fresh F.Formula
tTrans :: H.DataType -> [F.Formula]
trans  :: H.DefGeneral    -> [F.Formula]

type Fresh = State (String,Int)

eTrans (H.Var v) = F.Var v
eTrans (H.Fun f) = F.Fun f
eTrans (H.App e1 e2) = F.App (eTrans e1) (eTrans e2)
eTrans H.BAD = F.BAD
eTrans (H.Con d) = F.Var d

dTrans (H.Let f vs e) = F.foralls vs $ (eTransfxi f vs) `F.Eq` (eTrans e)
dTrans (H.LetCase f vs e pes) = F.foralls vs $ ((eTrans e `F.Eq` F.BAD) `F.Implies` (eTransfxi f vs `F.Eq` F.BAD)) `F.And` 
                                ((eTransfxi f vs `F.Eq` F.UNR) `F.Or` (foldl' (\f (d,_) -> f `F.Or` (((F.Fun "head") `F.App` eTrans e) `F.Eq` (F.Var (head d)))) F.False pes))

sTrans e H.Any = return F.True
sTrans e (H.Pred x u) = return $ (eTrans u' `F.Eq` F.UNR) `F.Or` ((F.CF $ eTrans e) `F.And` 
                                                                  (F.CF $ eTrans u') `F.And` (eTrans u' `F.Eq` (F.Var "True")))
  where u' = H.subst u e x
sTrans e (H.AppC x c1 c2) = do
  (s,k) <- get
  put (s,k+1)
  let freshX = s++(show k) 
      c2' = H.substC c2 (H.Var freshX) x
  f1 <- sTrans (H.Var freshX) c1
  f2 <- sTrans (H.App e (H.Var freshX)) c2'
  return $ F.Forall freshX (f1 `F.Implies` f2)

b = b

tTrans (H.Data d dns) = s1 ++ s2 ++ s3
  where s1 = map (\(d,a) -> F.foralls (xs a) $ (foldl (\f x -> ((F.Var ("sel" ++ x ++ d) `F.App` (foldl (\f y -> f `F.App` F.Var y) (F.Var d) (xs a))) `F.Eq` F.Var x) `F.And` f) F.True (xs a))) dns
        s2 = map (\((dx,ax),(dy,ay)) -> F.foralls (xs ax)  $ F.foralls (ys ay) $  F.Not $ (foldl (\ f x -> f `F.App` F.Var x)  (F.Var dx) (xs ax)) `F.Eq` (foldl (\ f x -> f `F.App` F.Var x)  (F.Var dy) (ys ay))) [(a,b) | a <- dns, b <- dns, a /= b]
        s3 = map (\(d,a) -> F.foralls (xs a) $ cfEq d (xs a)) dns
        xs a = map (\n -> "X"++(show n)) [1..a]
        ys a = map (\n -> "Y"++(show n)) [1..a]
        cfEq d xs = foldl (\f x -> F.CF (F.Var x) `F.And` f) F.True (xs) `F.Iff`
                      (F.CF $ foldl (\f x -> f `F.App` (F.Var x)) (F.Var d)  xs)
        

trans (H.Def d) = [dTrans d]
trans (H.DataType t) = tTrans t
trans (H.ContSat (H.Satisfies v c)) = [evalState (sTrans (H.Var v) c) ("Z",0)]

eTransfxi f vs = eTrans $ H.apps (H.Fun f:map H.Var vs)

okFromd :: H.Definition -> H.Contract
okFromd (H.Let _ vs _) = foldl' (\c _ -> H.AppC "dummy" c H.ok) H.ok vs

