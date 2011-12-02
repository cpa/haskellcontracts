{-# LANGUAGE DeriveFunctor, DeriveDataTypeable #-}

-- Types used in the FOL module.  Moved here to avoid cyclic
-- dependencies.
module Types.FOL (module Types.FOL, module Types.Haskell) where

import Data.Data

import Types.Haskell (Name,Named(..),Expression(..))

type Term = Expression

type Label = String
data LabeledFormula = LabeledFormula { getLabel :: Label, getFormula :: Formula }
                      deriving (Show,Eq,Data,Typeable)

infix 7 :<=>:
infix 7 :=>:
data Formula 
   = Forall [Name] Formula
   | Exists [Name] Formula
   | Formula :=>: Formula
   | Formula :<=>: Formula
   | Not Formula
   -- XXX: binary ':\/:' and ':/\:' would be more like
   -- TPTP
   | Or  [Formula]
   | And [Formula]
   -- XXX: do we have any need for Top and Bottom?
   | Top
   | Bottom
   | Term :=:  Term
   | Term :/=: Term
-- | Pred Name Term -- ^ Unary predicate. XXX: could unify CF and Min as Pred.
   | CF Term
   | Min Term
   deriving (Show,Eq,Data,Typeable)
-- cf = Pred "cf"
-- min = Pred "min"
