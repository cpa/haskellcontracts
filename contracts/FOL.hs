module FOL where

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

toTPTP :: Formula (Term Variable) -> String
toTPTP f = header ++ "\n" ++ (go f) ++ "\n" ++ footer
  where header = "fof(axiom,axiom,"
        footer = ").\n"
        go (Forall xs f) = "! " ++ show xs ++ "  : (" ++ go f ++ ")"
        go (Implies f1 f2) = "(" ++ go f1 ++ ") => (" ++ go f2 ++ ")"
        go (Iff f1 f2) = "(" ++ go f1 ++ ") <=> (" ++ go f2 ++ ")"
        go (Not (Eq t1 t2)) =  goTerm t1 ++ " != " ++ goTerm t2
        go (Not f) = "~(" ++ go f ++ ")"
        go (Or fs) = "(" ++ (concat $ intersperse " | " (map go fs)) ++ ")"
        go (And fs) = "(" ++ (concat $ intersperse " & " (map go fs)) ++ ")"
        go True = "$true"
        go False = "$false"
        go (Eq t1 t2) = goTerm t1 ++ " = " ++ goTerm t2
        go (CF t) = "cf(" ++ goTerm t ++ ")"
        goTerm (Var v) = show v
        goTerm (App []) = error "Cannot apply nothing"
        goTerm (App [t]) = goTerm t
        goTerm (App ts) = "app(" ++ goTerm (App (init ts)) ++ "," ++ goTerm (last ts) ++ ")"
        goTerm (FullApp f []) = show f
        goTerm (FullApp f as) = show f ++ "(" ++ (concat $ intersperse "," $ map show as) ++ ")"
        goTerm (Weak t) = "$weak(" ++ goTerm t ++")"
        -- TODO App efficiency fix