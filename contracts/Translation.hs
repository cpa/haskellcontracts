module Translation where

import qualified Haskell as H
import qualified FOL as F
import Data.List (foldl')

eTrans :: H.Expression -> F.Term
dTrans :: H.Definition -> F.Formula
sTrans :: H.Expression -> H.Contract -> F.Formula
trans  :: H.DefCont -> H.Contract -> F.Formula


eTrans (H.Var v) = F.Var v
eTrans (H.Fun f) = F.Constant $ F.Fun f
eTrans (H.Con c) = F.Constant $ F.Con c
eTrans (H.App e1 e2) = F.App (eTrans e1) (eTrans e2)
eTrans H.BAD = F.Constant $ F.BAD

dTrans (H.Let f vs e) = F.foralls vs $ (eTransfxi f vs) `F.Eq` (eTrans e)
dTrans (H.LetCase f vs e pes) = F.foralls vs $ ((eTrans e `F.Eq` F.Constant F.BAD) `F.Implies` (eTransfxi f vs `F.Eq` F.Constant F.BAD)) `F.And` 
                                ((eTransfxi f vs `F.Eq` (F.Constant F.UNR)) `F.Or` (foldl' (\f (d,_) -> f `F.Or` (((F.Constant $ F.Fun "head") `F.App` eTrans e) `F.Eq` (F.Constant $ F.Con (head d)))) F.False pes))

sTrans e H.Any = F.True
sTrans e (H.Pred x u) = (eTrans u' `F.Eq` F.Constant F.UNR) `F.Or` ((F.CF $ eTrans e) `F.And` (F.CF $ eTrans u') `F.And` (eTrans u' `F.Eq` (F.Constant $ F.Con "True")))
  where u' = H.subst u e x
sTrans e (H.AppC x c1 c2) = F.Forall freshX ((sTrans (H.Var freshX) c1) `F.Implies` (sTrans (H.App (H.Var x) (H.Var freshX)) c2))
  where freshX = "x17"

trans (H.Def d) = trans (H.Opaque d (okFromd d))
trans (H.Transp d c) = undefined
trans (H.Opaque d c) = undefined

eTransfxi f vs = eTrans $ H.apps (H.Fun f:map H.Var vs)

okFromd :: H.Definition -> H.Contract
okFromd (H.Let _ vs _) = foldl' (\c _ -> H.AppC "dummy" c H.ok) H.ok vs
