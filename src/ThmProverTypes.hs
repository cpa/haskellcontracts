{-# LANGUAGE DeriveDataTypeable, NamedFieldPuns #-}

-- Types used in the ThmProver module.  Moved here to avoid cyclic
-- dependencies.
module ThmProverTypes where

import Data.Typeable
import Data.Data

import HaskellTypes as H
import FOLTypes as F

data ThmProverConf = ThmProverConf
  { path :: FilePath
  , opts :: [String]
  , unsat  :: String -> Bool
  , theory :: Theory
  }

data Theory = Theory
  { showFormula :: [F.LabeledFormula] -> String
  , header :: [H.DefGeneral] -> String
  , fileExtension :: String
  , footer :: String
  }

data ThmProver
  = Equinox
  | SPASS
  | Vampire32
  | Vampire64
  | E
  | Z3
  deriving (Show, Data, Typeable, Bounded, Enum, Eq)
