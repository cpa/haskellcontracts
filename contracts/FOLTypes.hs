{-# LANGUAGE DeriveFunctor #-}

-- Types used in the FOL module.  Moved here to avoid cyclic
-- dependencies.
module FOLTypes (module FOLTypes, module HaskellTypes) where

import HaskellTypes (Name,Named,MetaNamed(..),Expression,MetaExpression(..))

type Term = Expression
type Formula = MetaFormula Term
type LabeledFormula = MetaLabeledFormula Formula
data MetaLabeledFormula a = LabeledFormula { getLabel :: Label, getFormula :: a }
                            deriving (Show, Eq, Functor)
type Label = String

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
--                 | Pred Name a -- ^ Unary predicate. XXX: could unify CF and Min as Pred.
                   | CF a
                   | Min a
                   deriving (Show,Eq,Functor)
-- cf = Pred "cf"
-- min = Pred "min"
exists xs phi = Not $ Forall xs $ Not phi
