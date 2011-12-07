{-# LANGUAGE DeriveFunctor, DeriveDataTypeable #-}

-- Types used in the FOL module.  Moved here to avoid cyclic
-- dependencies.
module Types.FOL (module Types.FOL, module Types.Haskell) where

import Data.Data

import Types.Haskell (Name,Named(..),Expression(..),GetName(..))

-- | Goal / assumption distinction for formulas.
data Variance
-- Better to just call these 'Goal' and 'Assumption' ?
   = Plus  -- ^ Goal
   | Minus -- ^ Assumption
     deriving (Show,Eq,Data,Typeable)

instance GetName Label where
  getName = getNameLabel

data Label = Label { getNameLabel :: Name, getVariance :: Variance }
             deriving (Show,Eq,Data,Typeable)
data LabeledFormula = LabeledFormula { getLabel :: Label, getFormula :: Formula }
                      deriving (Show,Eq,Data,Typeable)

-- | Convenience functions for making labels.
axiom, theorem :: Name -> Label
axiom   n = Label n Minus
theorem n = Label n Plus

type Term = Expression
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
