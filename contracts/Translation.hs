import Control.Monad.State

import qualified Expr as E
import qualified FOL  as F
import Program 
import qualified Contract as C

type Unique = State Int

exprFol :: E.Expr -> F.FOL
exprFol (E.Var v) = F.Var v Nothing
exprFol (E.Con c (Just e)) = F.Var c (Just $ exprFol e)
exprFol (E.Con c Nothing) = F.Var c Nothing
exprFol (E.App e1 e2) = case a1 of 
  Nothing -> F.Var v1 (Just f2)
  Just f  -> F.Var v1 (Just $ F.plug f f2)
  where F.Var v1 a1 = exprFol e1
        f2 = exprFol e2

exprToFol :: E.Expr -> C.Contract -> Unique F.FOL
exprToFol e (C.Pred _ p) = return $ exprFol (p `E.App` e)
exprToFol e (C.App t1 t2) = do
  n <- get
  let v = "x" ++ (show n)
  put (n+1)
  f1 <- exprToFol (E.Var v) t1
  f2 <- exprToFol (e `E.App` E.Var v) t2
  return $ F.Forall v (f1 `F.Implies` f2)
  where v = "x0"
exprToFol e C.Any = return $ F.Top

progToFol :: Program -> Unique F.FOL
progToFol (Let f as e t) = do
  f1 <- exprToFol (E.Var f) t
  f2 <- exprToFol e tc
  f3 <- xiInti as t
  return $ foldr (\ a f -> F.Forall a f) (f3 `F.And` (F.Forall f f1) `F.Implies` f2) as
  where tc = C.codomain t
progToFol (LetCase f as e pes t) = do
  f1 <- exprToFol (E.Var f) t
  f2 <- rhs e pes tc
  f3 <- xiInti as t
  return $ foldr (\ a f -> F.Forall a f) (f3 `F.And` (F.Forall f f1) `F.Implies` f2) as
  where tc = C.codomain t

rhs :: E.Expr -> [(Pattern,E.Expr)] -> C.Contract -> Unique F.FOL
rhs e pes t = do 
    foldr (\ (pj,ej) f -> do df <- f
                             liftM (\w -> ((eqPat e pj) `F.Implies` w) `F.And` df) (exprToFol ej t)) (return F.Top) pes

eqPat :: E.Expr -> Pattern -> F.FOL
eqPat e (vp,vps) = exprFol e `F.Eq` (foldr (\v f -> F.Var v (Just f)) (F.Var vp Nothing) vps)

xiInti :: [E.Variable] -> C.Contract -> Unique F.FOL
xiInti [] _ = return $ F.Top -- TODO : it's wrong
xiInti (x:xs) (C.App t ts) = do 
  f1 <- exprToFol (E.Var x) t 
  f2 <- xiInti xs ts
  return $ f1 `F.And` f2


  
        
test = [ exprToFol (E.Var "x") C.Any
       , exprToFol (E.App (E.Var "f") $ E.Var "y") C.Any
       , exprToFol (E.Con "D" $ Just $ E.Var "f") C.dummy
       , progToFol (Let "g" ["x","y"] (E.Var "True") (C.dummy `C.App` (C.dummy `C.App` C.dummy)))]
       
test2 = progToFol (Let "g" ["x"] (E.Var "True") (C.dummy `C.App` C.dummy))
