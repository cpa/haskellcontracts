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


data Formula a = Forall [a] (Formula a)
               | Implies (Formula a) (Formula a)
               | Iff (Formula a) (Formula a)
               | Not (Formula a)
               | Or [Formula a]
               | And [Formula a]
               | True
               | False
               | Eq a a
               | CF a
               deriving (Show,Eq)
                        
instance Functor Formula where
  fmap g (Forall xs f) = Forall (fmap g xs) (fmap g f)
  fmap g (Implies f1 f2) = Implies (fmap g f1) (fmap g f2)
  fmap g (Iff f1 f2) = Iff (fmap g f1) (fmap g f2)
  fmap g (Not f) = Not (fmap g f)
  fmap g (Or fs) = Or (map (fmap g) fs)
  fmap g (And fs) = And (map (fmap g) fs)
  fmap g (Eq t1 t2) = Eq (g t1) (g t2)
  fmap g (CF t) = CF (g t)
  fmap g True = True
  fmap g False = False
  
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
removeConstants (Implies False _) = True
removeConstants (Iff True f) = f
removeConstants (Iff f True) = f
removeConstants (Iff False f) = Not f
removeConstants (Iff f False) = Not f
removeConstants (Not f) = Not $ removeConstants f
removeConstants (Or fs) = if any (==True) fs then True else Or $ filter (/=False) fs
removeConstants (And fs) = if any (==False) fs then False else And $ filter (/=True) fs
removeConstants f = f

simplify f = filter (/= True) $ splitOnAnd (removeConstants f)

extractVR (Var (Regular x)) = x

upperIfy :: [String] -> Formula (Term Variable) -> Formula (Term Variable)
upperIfy c (Forall xs f) = Forall (map (Var . Regular . map toUpper . extractVR) xs) (upperIfy c' f)
  where c' = (map extractVR xs)++c
upperIfy c (Iff f1 f2) = Iff (upperIfy c f1) (upperIfy c f2)
upperIfy c (Implies f1 f2) = Implies (upperIfy c f1) (upperIfy c f2)
upperIfy c (Not f) = Not (upperIfy c f)
upperIfy c True = True
upperIfy c False = False
upperIfy c (Eq t1 t2) = Eq (auxUpper c t1) (auxUpper c t2)
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
        aux (Implies f1 f2) = "(" ++ aux f1 ++ ") => (" ++ aux f2 ++ ")"
        aux (Iff f1 f2) = "(" ++ aux f1 ++ ") <=> (" ++ aux f2 ++ ")"
        aux (Not (Eq t1 t2)) =  auxTerm t1 ++ " != " ++ auxTerm t2
        aux (Not f) = "~(" ++ aux f ++ ")"
        aux (Or fs) = "(" ++ (concat $ intersperse " | " (map aux fs)) ++ ")"
        aux (And fs) = "(" ++ (concat $ intersperse " & " (map aux fs)) ++ ")"
        aux True = "$true"
        aux False = "$false"
        aux (Eq t1 t2) = auxTerm t1 ++ " = " ++ auxTerm t2
        aux (CF t) = "cf(" ++ auxTerm t ++ ")"
        auxTerm (Var v) = show v
        auxTerm (App []) = error "Cannot apply nothing"
        auxTerm (App [t]) = auxTerm t
        auxTerm (App ts) = "app(" ++ auxTerm (App (init ts)) ++ "," ++ auxTerm (last ts) ++ ")"
        auxTerm (FullApp f []) = show f
        auxTerm (FullApp f as) = show f ++ "(" ++ (concat $ intersperse "," $ map show as) ++ ")"
        auxTerm (Weak t) = "$weak(" ++ auxTerm t ++")"
