module FOL ( Term (..)
           , Constant (..)
           , Formula (..)
           , foralls  
           , apps)
where

type Variable = String

data Term = Var Variable
          | App Term Term
          | Constant Constant
          deriving (Show,Eq)                  
       
data Constant = Con Variable
              | Fun Variable
              | BAD
              | UNR
              deriving (Show,Eq)                  
      
data Formula = Forall Variable Formula
             | Implies Formula Formula
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

apps [] = error "Cannot apply nothing"
apps [x] = x
apps (x:xs) = x `App` (apps xs)