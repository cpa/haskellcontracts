module FOL ( Term (..)
           , Formula (..)
           , foralls  
           , apps
           , toTPTP
           , simplify
           , toLatex
           , toLatexDocument)
       where

import Prelude hiding (True,False)

type Variable = String

data Term = Var Variable
          | App Term Term
          | Fun Variable
          | BAD
          | UNR
          deriving (Show,Eq)                  
      
data Formula = Forall Variable Formula
             | Implies Formula Formula
             | Iff Formula Formula
             | Not Formula
             | Or Formula Formula
             | And Formula Formula 
             | True
             | False
             | Eq Term Term
             | CF Term
             deriving (Show,Eq)                  

foralls [] f = f
foralls (x:xs) f = Forall x (foralls xs f)

apps xs = foldl1 (\w x -> App w x) xs


splitOnAnd :: Formula -> [Formula]
splitOnAnd (Forall x (And f1 f2)) = splitOnAnd (Forall x f1) ++ splitOnAnd (Forall x f2)
splitOnAnd (Forall x f) = map (Forall x) $ splitOnAnd f
splitOnAnd (And f1 f2) = splitOnAnd f1 ++ splitOnAnd f2
splitOnAnd f = [f]

removeConstants :: Formula -> Formula
removeConstants (Forall x f) = Forall x (removeConstants f)
removeConstants (Implies False f2) = True
removeConstants (Iff True f) = f
removeConstants (Iff f True) = f
removeConstants (Iff False f) = Not f
removeConstants (Iff f False) = Not f
removeConstants (Not f) = Not $ removeConstants f
removeConstants (Or f False) = removeConstants f
removeConstants (Or False f) = removeConstants f
removeConstants (Or True f) = True
removeConstants (Or f True) = True
removeConstants (Or f1 f2) = Or (removeConstants f1) (removeConstants f2)
removeConstants (And f True) = removeConstants f
removeConstants (And True f) = removeConstants f
removeConstants (And f False) = False
removeConstants (And False f) = False
removeConstants (And f1 f2) = And (removeConstants f1) (removeConstants f2)
removeConstants f = f

simplify = filter (/= True) . splitOnAnd . removeConstants


toTPTP :: Formula -> String
toTPTP f = header ++ "\n" ++ (aux f) ++ "\n" ++ footer
  where header = "fof(axiom,axiom,"
        footer = ").\n"
        aux (Forall v f) = "![ " ++ (map id v) ++ " ] : (" ++ aux f ++ ")"
        aux (Implies f1 f2) = "(" ++ aux f1 ++ ") => (" ++ aux f2 ++ ")"
        aux (Iff f1 f2) = "(" ++ aux f1 ++ ") <=> (" ++ aux f2 ++ ")"
        aux (Not f) = "~(" ++ aux f ++ ")"
        aux (Or f1 f2) = "(" ++ aux f1 ++ ") | (" ++ aux f2 ++ ")"
        aux (And f1 f2) = "(" ++ aux f1 ++ ") & (" ++ aux f2 ++ ")"
        aux True = "$true"
        aux False = "$false"
        aux (Eq t1 t2) = "((" ++ auxTerm t1 ++ ") = (" ++ auxTerm t2 ++ "))"
        aux (CF t) = "cf(" ++ auxTerm t ++ ")"
        
        auxTerm (Var v) = v
        auxTerm (App t1 t2) = "( app(" ++ auxTerm t1 ++ "," ++ auxTerm t2 ++ "))"
        auxTerm (Fun f) = f
        auxTerm BAD = "bad"
        auxTerm UNR = "unr"

toLatex (Forall v f) = " \\forall " ++ v ++ ". " ++ toLatex f
toLatex (Implies f1 f2) = "(" ++ toLatex f1 ++ " \\implies " ++ toLatex f2 ++ ")"
toLatex (Iff f1 f2) = "(" ++ toLatex f1 ++ " \\iff " ++ toLatex f2 ++ ")"
toLatex (Not f) = " \\lnot " ++ toLatex f
toLatex (Or f1 f2) = "(" ++ toLatex f1 ++ " \\lor " ++ toLatex f2 ++ ")"
toLatex (And f1 f2) = "(" ++ toLatex f1 ++ " \\land " ++ toLatex f2 ++ ")"
toLatex True = " \\top "
toLatex False = " \\bot "
toLatex (Eq t1 t2) = toLatexTerm t1 ++ " = " ++ toLatexTerm t2
toLatex (CF t) = " \\mbox{CF}(" ++ toLatexTerm t ++ ") "

toLatexTerm (Var v) = v
toLatexTerm (App t1 t2) = toLatexTerm t1 ++ "(" ++ toLatexTerm t2 ++ ")"
toLatexTerm (Fun v) = v
toLatexTerm (BAD) = " \\bad "
toLatexTerm (UNR) = " \\unr "

toLatexDocument f = "\\documentclass{article}\n\n\\usepackage{stmaryrd}\n\\usepackage{amsmath}\n\\usepackage{fullpage}\n\n\\begin{document}\n\n\\newcommand{\\unr}{\\texttt{UNR}}\n\\newcommand{\\bad}{\\texttt{BAD}}\n\\newcommand{\\any}{\\texttt{Any}}\n\\newcommand{\\ok}{\\texttt{Ok}}\n\n\\thispagestyle{empty}\n $ " ++ toLatex f ++ "$ \n \\end{document}\n"