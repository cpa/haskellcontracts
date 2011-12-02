module Types.Translation (module Types.Translation, module Types.FOL) where

import Control.Monad.State

import Types.FOL (LabeledFormula,Name)

type Arity = (Name,Int)

type Fresh = State TransState
data TransState = S { prefix  :: String -- the prefix of our fresh variables
                    , count   :: Int    -- a counter for the suffix of our fresh variables
                    , arities :: [Arity] -- The arities of functions/data constructors in the program, which should be read-only
                      -- We save this up during processing because we
                      -- need to conjoin all at the end.  An
                      -- alternative would be tag formulas with their
                      -- Plus / Minus status.
                    , getGoals :: [LabeledFormula] -- ^ translated goal contracts
                    }
