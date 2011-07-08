module FOL where

import Prelude hiding (True,False)
import Data.Char (toUpper)
import Data.List (intersperse)

data Variable = Regular String
              | BAD
              | UNR
              deriving (Eq)
instance Show Variable where
  show (Regular v) = v
  show BAD = "bad"
  show UNR = "unr"

data Term a = Var a
            | App [Term a]
            | FullApp a [Term a]
            | Weak (Term a)
            deriving (Eq)

instance Show a => Show (Term a) where
  show (Var v) = show v
  show (App []) = error "Cannot apply nothing"
  show (App [t]) = show t
  show (App ts) = "app(" ++ show (App (init ts)) ++ "," ++ show (last ts) ++ ")"
  show (Weak v) = show v
  show (FullApp f []) = show f
  show (FullApp f as) = show f ++ "(" ++ (concat $ intersperse "," $ map show as) ++ ")"

infix 7 :<=>:
infix 7 :=>:
data Formula a = Forall [a] (Formula a)
               | (:=>:) (Formula a) (Formula a)
               | (:<=>:) (Formula a) (Formula a)
               | Not (Formula a)
               | Or [Formula a]
               | And [Formula a]
               | Top
               | Bottom
               | (:=:) a a
               | (:/=:) a a
               | CF a
               deriving (Show,Eq)
                        
instance Functor Formula where
  fmap g (Forall xs f) = Forall (fmap g xs) (fmap g f)
  fmap g (f1 :=>: f2) = (fmap g f1) :=>: (fmap g f2)
  fmap g (f1 :<=>: f2) = (fmap g f1) :<=>:(fmap g f2)
  fmap g (Not f) = Not (fmap g f)
  fmap g (Or fs) = Or (map (fmap g) fs)
  fmap g (And fs) = And (map (fmap g) fs)
  fmap g (t1 :=: t2) = (g t1) :=: (g t2)
  fmap g (t1 :/=: t2) = (g t1) :/=: (g t2)
  fmap g (CF t) = CF (g t)
  fmap g Top = Top
  fmap g Bottom = Bottom
  
splitOnAnd :: Formula a -> [Formula a]
splitOnAnd (Forall xs (And fs)) = map (Forall xs) fs
splitOnAnd (Forall xs f) = map (Forall xs) $ splitOnAnd f
splitOnAnd (And fs) = concatMap splitOnAnd fs
splitOnAnd f = [f]

appifyFOF a f = fmap (\x -> case x of App ((Var x):xs) -> case lookup x a of
                                        Just n -> if n == length xs then FullApp x xs else App (Var x : xs)
                                        Nothing -> App (Var x : xs)
                                      x -> x) f

removeConstants :: Eq a => Formula a -> Formula a
removeConstants (Forall [] f) = removeConstants f
removeConstants (Forall xs f) = Forall xs (removeConstants f)
removeConstants (Bottom :=>: _) = Top
removeConstants (Top :<=>: f) = f
removeConstants (f :<=>: Top) = f
removeConstants (Bottom :<=>: f) = Not f
removeConstants (f :<=>: Bottom) = Not f
removeConstants (Not f) = Not $ removeConstants f
removeConstants (Or fs) = if any (==Top) fs then Top else Or $ filter (/=Bottom) fs
removeConstants (And fs) = if any (==Bottom) fs then Bottom else And $ filter (/=Top) fs
removeConstants f = f

simplify f = filter (/= Top) $ splitOnAnd (removeConstants f)

extractVR (Var (Regular x)) = x

upperIfy :: [String] -> Formula (Term Variable) -> Formula (Term Variable)
upperIfy c (Forall xs f) = Forall (map (Var . Regular . map toUpper . extractVR) xs) (upperIfy c' f)
  where c' = (map extractVR xs)++c
upperIfy c (f1 :<=>: f2) = (upperIfy c f1) :<=>: (upperIfy c f2)
upperIfy c (f1 :=>: f2) = (upperIfy c f1) :=>: (upperIfy c f2)
upperIfy c (Not f) = Not (upperIfy c f)
upperIfy c Top = Top
upperIfy c Bottom = Bottom
upperIfy c (t1 :=: t2) = (auxUpper c t1) :=: (auxUpper c t2)
upperIfy c (t1 :/=: t2) = (auxUpper c t1) :/=: (auxUpper c t2)
upperIfy c (CF t) = CF (auxUpper c t)
upperIfy c (And fs) = And (map (upperIfy c) fs)
upperIfy c (Or fs) = Or (map (upperIfy c) fs)


auxUpper :: [String] -> Term Variable -> Term Variable
auxUpper c (Var (Regular v)) = if v `elem` c then (Var . Regular) (map toUpper v) else Var $ Regular v
auxUpper c (Var v) = Var v
auxUpper c (App ts) = App $ map (auxUpper c) ts
auxUpper c (FullApp x ts) = FullApp x (map (auxUpper c) ts) -- TODO think about it
auxUpper c (Weak t) = Weak $ auxUpper c t

toTPTP :: Formula (Term Variable) -> String
toTPTP f = header ++ "\n" ++ (aux $ upperIfy [] f) ++ "\n" ++ footer
  where header = "fof(axiom,axiom,"
        footer = ").\n"
        aux (Forall xs f) = "! " ++ show xs ++ "  : (" ++ aux f ++ ")"
        aux (f1 :=>: f2) = "(" ++ aux f1 ++ ") => (" ++ aux f2 ++ ")"
        aux (f1 :<=>: f2) = "(" ++ aux f1 ++ ") <=> (" ++ aux f2 ++ ")"
        aux (Not (t1 :=: t2)) =  auxTerm t1 ++ " != " ++ auxTerm t2
        aux (Not f) = "~(" ++ aux f ++ ")"
        aux (Or fs) = "(" ++ (concat $ intersperse " | " (map aux fs)) ++ ")"
        aux (And fs) = "(" ++ (concat $ intersperse " & " (map aux fs)) ++ ")"
        aux Top = "$true"
        aux Bottom = "$false"
        aux (t1 :=: t2) = auxTerm t1 ++ " = " ++ auxTerm t2
        aux (t1 :/=: t2) = auxTerm t1 ++ " != " ++ auxTerm t2
        aux (CF t) = "cf(" ++ auxTerm t ++ ")"
        auxTerm (Var v) = show v
        auxTerm (App []) = error "Cannot apply nothing"
        auxTerm (App [t]) = auxTerm t
        auxTerm (App ts) = "app(" ++ auxTerm (App (init ts)) ++ "," ++ auxTerm (last ts) ++ ")"
        auxTerm (FullApp f []) = show f
        auxTerm (FullApp f as) = show f ++ "(" ++ (concat $ intersperse "," $ map show as) ++ ")"
        auxTerm (Weak t) = "$weak(" ++ auxTerm t ++")"
