{-# LANGUAGE DeriveDataTypeable, NamedFieldPuns #-}

-- Types used in the ThmProver module.  Moved here to avoid cyclic
-- dependencies.
module Types.ThmProver where

import Data.Typeable
import Data.Data

import Types.Haskell as H
import Types.FOL as F

data ThmProverConf = ThmProverConf
  { path :: FilePath
  , opts :: [String]
  , unsat  :: String -> Bool
  , theory :: Theory
  }

data Theory = Theory
  { showFormula :: [F.LabeledFormula] -> String
  , header :: [H.TopLevelStatement] -> String
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
