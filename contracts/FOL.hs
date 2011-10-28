{-# LANGUAGE DeriveFunctor #-}

module FOL (module FOL, module Haskell) where

--import qualified Haskell as H
import Haskell (Name,Named(..),Expression,MetaExpression(..),Arity,appifyExpr,getName)
import Debug.Trace
import Data.Char (toUpper)
import Data.List (intercalate)

type Term = Expression
type Formula = MetaFormula Term

-- instance Show a => Show (MetaTerm a) where
--   show (Var v) = show v
--   show (App []) = error "Cannot apply nothing"
--   show (App [t]) = show t
--   show (App ts) = "app(" ++ show (App (init ts)) ++ "," ++ show (last ts) ++ ")"
--   show (FullApp f []) = show f
--   show (FullApp f as) = show f ++ "(" ++ (intercalate "," $ map show as) ++ ")"

infix 7 :<=>:
infix 7 :=>:
data MetaFormula a = Forall [Name] (MetaFormula a)
                   | (MetaFormula a) :=>: (MetaFormula a)
                   | (MetaFormula a) :<=>: (MetaFormula a)
                   | Not (MetaFormula a)
                   -- XXX: binary ':\/:' and ':/\:' would be more like
                   -- TPTP
                   | Or [MetaFormula a]
                   | And [MetaFormula a]
                   -- XXX: do we have any need for Top and Bottom?
                   | Top
                   | Bottom
                   | a :=: a
                   | a :/=: a
                   | CF a
                   -- | Min a
                   deriving (Show,Eq,Functor)


-- forall a . x && y --> (forall a . x) && (forall a . y)
splitOnAnd :: Formula -> [Formula]
splitOnAnd (Forall xs (And fs)) = map (Forall xs) fs
splitOnAnd (Forall xs f) = map (Forall xs) $ splitOnAnd f
splitOnAnd (And fs) = concatMap splitOnAnd fs
splitOnAnd f = [f]

removeConstants :: Formula -> Formula
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

simplify f = filter (/= Top) $ splitOnAnd $ removeConstants f


toTPTP :: Formula -> String
toTPTP f = header ++ "\n" ++ go f ++ "\n" ++ footer
  -- XXX, MAYBE TODO: add better header or comments. Right now the first "axiom"
  -- below is the name of the axiom.  The TPTP format also allows optional 4th and
  -- 5th fields for comments.  Would be nice to see each formula labeled with its
  -- type or source, to make debugging the generated .tptp file easier.
  where header = "fof(axiom,axiom,"
        footer = ").\n"
        -- XXX, ???: do upper casing here instead?  Maybe by adding a
        -- QVar to Named?
        go (Forall xs f) = "! " ++ show (map quantified xs) ++ "  : (" ++ go f ++ ")"
        go (f1 :=>: f2) = "(" ++ go f1 ++ ") => (" ++ go f2 ++ ")"
        go (f1 :<=>: f2) = "(" ++ go f1 ++ ") <=> (" ++ go f2 ++ ")"
        go (Not (t1 :=: t2)) =  goTerm t1 ++ " != " ++ goTerm t2
        go (Not f) = "~(" ++ go f ++ ")"
        go (Or []) = error "add suport for empty OR becomes false"
        go (Or fs) = "(" ++ (intercalate " | " (map go fs)) ++ ")"
        go (And []) = "$true"
        go (And fs) = "(" ++ (intercalate " & " (map go fs)) ++ ")"
        go Top = "$true"
        go Bottom = "$false"
        go (t1 :=: t2) = goTerm t1 ++ " = " ++ goTerm t2
        go (t1 :/=: t2) = goTerm t1 ++ " != " ++ goTerm t2
        go (CF t) = "cf(" ++ goTerm t ++ ")"

        goTerm (Named n) = goNamed n
        goTerm (e1 :@: e2) = "app(" ++ goTerm e1 ++ "," ++ goTerm e2 ++ ")"
        goTerm (FullApp f []) = goNamed f
        goTerm (FullApp f as) = full f ++ "(" ++ (intercalate "," $ map show as) ++ ")"

        goNamed (Var v) = v
        goNamed (Rec v) = v ++ "_rec"
        goNamed (Con v) = "'" ++ v ++ "'"
        goNamed (QVar v) = quantified v

        full = ("full_"++) . getName
        quantified = map toUpper

-- takes formulas and a list of arities for each definition
-- and returns those formulas using "full application" wherever possible
appifyF :: [Arity] -> [Formula] -> [Formula]
appifyF a fs = map (fmap $ appifyExpr a) fs
