module FOL where

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
         | Var Variable (Maybe FOL)
         | Eq FOL FOL
         | BAD | UNR
         deriving (Eq,Show)

plug :: FOL -> FOL -> FOL
plug (Var v Nothing) f = Var v (Just f)
plug (Var v (Just f1)) f2 = Var v (Just (plug f1 f2))


simplify :: FOL -> FOL
simplify (And Top x) = x
simplify (And x Top) = x
simplify (Implies a b) = (Not (simplify a)) `Or` simplify b
simplify (Not (Not x)) = x
simplify (Not x) = Not $ simplify x
simplify (And x y) = Not $ (Not x) `Or` (Not y)
simplify (Forall x y) = Forall x (simplify y)
simplify (Or x y) = Or (simplify x) (simplify y)
simplify (Var x a) = Var x a
simplify (Eq x y) = Eq (simplify x) (simplify y)
simplify x = x
