module FOL
       where

import Prelude hiding (True,False)
import Data.Char (toUpper)
import Data.Traversable
import Control.Applicative
import qualified Data.Foldable as F
import Data.Functor
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
            | Weak (Term a)
            deriving (Eq)

instance Show a => Show (Term a) where
  show (Var v) = show v
  show (App v) = show v
  show (Weak v) = show v
      
instance Functor Term where
  fmap f (Var v) = Var (f v)
  fmap f (App terms) = App (map (fmap f) terms)
  
instance F.Foldable Term where
  foldr f z (Var v) = f v z
  foldr f z (App []) = z
  foldr f z (App (Var v:as)) = f v (F.foldr f z (App as))
  foldr f z (App (App as1:as2)) = F.foldr f (F.foldr f z (App as1)) (App as2)


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

instance F.Foldable Formula where
  foldr = undefined
  
instance Traversable Formula where
  traverse f (Forall vs fof) = undefined
  traverse f (Implies fof1 fof2) = Implies <$> (traverse f fof1) <*> (traverse f fof2)
  traverse f (Iff fof1 fof2) = Iff <$> (traverse f fof1) <*> (traverse f fof2)
  traverse f (Not fof) = Not <$> traverse f fof
  traverse f (Or fofs) = Or <$> sequenceA (map (traverse f) fofs)
  traverse f (And fofs) = And <$> sequenceA (map (traverse f) fofs)
  traverse f True = pure True
  traverse f False = pure False
  traverse f (Eq term1 term2) = Eq <$> (f term1) <*> (f term2)
  traverse f (CF term) = CF <$> f term

instance Functor Formula where
  fmap f (Forall xs fof) = Forall (map f xs) (fmap f fof)
  fmap f (Implies fof1 fof2) = Implies (fmap f fof1) (fmap f fof2)
  fmap f (Iff fof1 fof2) = Iff (fmap f fof1) (fmap f fof2)
  fmap f (Not fof) = Not (fmap f fof)
  fmap f (Or fofs) = Or (map (fmap f) fofs)
  fmap f (And fofs) = And (map (fmap f) fofs)
  fmap f True = True
  fmap f False = False
  fmap f (Eq term1 term2) = Eq (f term1) (f term2)
  fmap f (CF term) = CF (f term) 
  
splitOnAnd :: Formula a -> [Formula a]
splitOnAnd (Forall xs (And fs)) = map (Forall xs) fs
splitOnAnd (Forall xs f) = map (Forall xs) $ splitOnAnd f
splitOnAnd (And fs) = concatMap splitOnAnd fs
splitOnAnd f = [f]

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
upperIfy c (Implies f1 f2) = Implies (upperIfy c f1) (upperIfy c f2)
upperIfy c (Iff f1 f2) = Iff (upperIfy c f1) (upperIfy c f2)
upperIfy c (Not f) = Not (upperIfy c f)
upperIfy c (And fs) = And (map (upperIfy c) fs)
upperIfy c (Or fs) = Or (map (upperIfy c) fs)
upperIfy c True = True
upperIfy c False = False
upperIfy c (Eq t1 t2) = Eq (auxUpper c t1) (auxUpper c t2)
upperIfy c (CF t) = CF (auxUpper c t)

auxUpper :: [String] -> Term Variable -> Term Variable
auxUpper c (Var (Regular v)) = if v `elem` c then (Var . Regular) (map toUpper v) else Var $ Regular v
auxUpper c (Var v) = Var v
auxUpper c (App ts) = App $ map (auxUpper c) ts
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
        auxTerm (Weak t) = "$weak(" ++ auxTerm t ++")"
        -- TODO App fix
-- toLatex (Forall v f) = " \\forall " ++ v ++ ". " ++ toLatex f
-- toLatex (Implies f1 f2) = "(" ++ toLatex f1 ++ " \\implies " ++ toLatex f2 ++ ")"
-- toLatex (Iff f1 f2) = "(" ++ toLatex f1 ++ " \\iff " ++ toLatex f2 ++ ")"
-- toLatex (Not f) = " \\lnot " ++ toLatex f
-- toLatex (Or f1 f2) = "(" ++ toLatex f1 ++ " \\lor " ++ toLatex f2 ++ ")"
-- toLatex (And f1 f2) = "(" ++ toLatex f1 ++ " \\land " ++ toLatex f2 ++ ")"
-- toLatex True = " \\top "
-- toLatex False = " \\bot "
-- toLatex (Eq t1 t2) = toLatexTerm t1 ++ " = " ++ toLatexTerm t2
-- toLatex (CF t) = " \\mbox{CF}(" ++ toLatexTerm t ++ ") "

-- toLatexTerm (Var v) = v
-- toLatexTerm (App t1 t2) = toLatexTerm t1 ++ "(" ++ toLatexTerm t2 ++ ")"
-- toLatexTerm (Fun v) = v
-- toLatexTerm (BAD) = " \\bad "
-- toLatexTerm (UNR) = " \\unr "

-- toLatexDocument f = "\\documentclass{article}\n\n\\usepackage{stmaryrd}\n\\usepackage{amsmath}\n\\usepackage{fullpage}\n\n\\begin{document}\n\n\\newcommand{\\unr}{\\texttt{UNR}}\n\\newcommand{\\bad}{\\texttt{BAD}}\n\\newcommand{\\any}{\\texttt{Any}}\n\\newcommand{\\ok}{\\texttt{Ok}}\n\n\\thispagestyle{empty}\n $ " ++ toLatex f ++ "$ \n \\end{document}\n"
