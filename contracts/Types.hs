import System.Random
import Control.Monad (liftM)

type Variable = String
type Constructor = String

data FOL = Top
         | Bottom
         | And FOL FOL
         | Or FOL FOL
         | Not FOL
         | Implies FOL FOL 
         | Iff FOL FOL
         | Forall Variable FOL
         | VarF Variable (Maybe FOL)
         | Eq FOL FOL
         | BADF | UNRF
         deriving (Eq,Show)


data Expr = Var Variable
          | App Expr Expr
          | Con Constructor (Maybe Expr)
          | BAD | UNR
          deriving (Eq,Show)
                  
data Program = Let Variable [Variable] Expr Contract
             | LetCase Variable [Variable] Expr [(Pattern,Expr)] Contract
             deriving (Eq,Show)
               
type Pattern = (Constructor,[Variable])

data Contract = Pred Variable Expr -- {var|expr(var)}
              | AppC Contract Contract
              | Any
              deriving (Eq,Show)
ok = Pred "x" (Var "f")

plug :: FOL -> FOL -> FOL
plug (VarF v Nothing) f = VarF v (Just f)
plug (VarF v (Just f1)) f2 = VarF v (Just (plug f1 f2))

exprFol :: Expr -> FOL
exprFol (Var v) = VarF v Nothing
exprFol (Con c (Just e)) = VarF c (Just $ exprFol e)
exprFol (Con c Nothing) = VarF c Nothing
exprFol (App e1 e2) = case a1 of 
  Nothing -> VarF v1 (Just f2)
  Just f  -> VarF v1 (Just $ plug f f2)
  where VarF v1 a1 = exprFol e1
        f2 = exprFol e2

exprToFol :: Expr -> Contract -> IO FOL
exprToFol e (Pred _ p) = return $ exprFol (p `App` e)
exprToFol e (AppC t1 t2) = do
  az <- randomIO :: IO Int
  let v = "x"++(show (az `mod` 10))
  f1 <- exprToFol (Var v) t1
  f2 <- exprToFol (e `App` Var v) t2
  return $ Forall v (f1 `Implies` f2)
  where v = "x0"
exprToFol e Any = return $ Top

progToFol :: Program -> IO FOL
progToFol (Let f as e t) = do
  f1 <- exprToFol (Var f) t
  f2 <- exprToFol (subst f f e) tc
  f3 <- xiInti as t
  return $ foldr (\ a f -> Forall a f) (f3 `And` (Forall f f1) `Implies` f2) as
  where tc = codomain t
progToFol (LetCase f as e pes t) = do
  f1 <- exprToFol (Var f) t
  f2 <- rhs e pes tc
  f3 <- xiInti as t
  return $ foldr (\ a f -> Forall a f) (f3 `And` (Forall f f1) `Implies` f2) as
  where tc = codomain t

subst _ _ x = x
        
codomain (Pred a b) = Pred a b
codomain Any = Any
codomain (AppC t1 t2) = codomain t2

rhs :: Expr -> [(Pattern,Expr)] -> Contract -> IO FOL
rhs e pes t = do 
    foldr (\ (pj,ej) f -> do df <- f
                             liftM (\w -> ((eqPat e pj) `Implies` w) `And` df) (exprToFol ej t)) (return Top) pes

eqPat :: Expr -> Pattern -> FOL
eqPat e (vp,vps) = exprFol e `Eq` (foldr (\v f -> VarF v (Just f)) (VarF vp Nothing) vps)


xiInti :: [Variable] -> Contract -> IO FOL
xiInti [] _ = return $ Top -- TODO : it's wrong
xiInti (x:xs) (AppC t ts) = do 
  f1 <- exprToFol (Var x) t 
  f2 <- xiInti xs ts
  return $ f1 `And` f2
  
        
test = [ exprToFol (Var "x") Any
       , exprToFol (App (Var "f") $ Var "y") Any
       , exprToFol (Con "D" $ Just $ Var "f") ok
       , progToFol (Let "g" ["x","y"] (Var "True") (ok `AppC` (ok `AppC` ok)))]
       
test2 = progToFol (Let "g" ["x"] (Var "True") (ok `AppC` ok))

simplify :: FOL -> FOL
simplify (And Top x) = x
simplify (And x Top) = x
simplify (Implies a b) = (Not (simplify a)) `Or` simplify b
simplify (Not (Not x)) = x
simplify (Not x) = Not $ simplify x
simplify (And x y) = Not $ (Not x) `Or` (Not y)
simplify (Forall x y) = Forall x (simplify y)
simplify (Or x y) = Or (simplify x) (simplify y)
simplify (VarF x a) = VarF x a
simplify (Eq x y) = Eq (simplify x) (simplify y)
simplify x = x