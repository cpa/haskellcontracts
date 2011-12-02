{-# LANGUAGE DeriveFunctor, DeriveDataTypeable #-}

-- Types used in the Haskell module.  Moved here to avoid cyclic
-- dependencies.
module Types.Haskell where

import Data.Generics (Data,Typeable)

type Pattern = (Name, [Name])
type Program = [TopLevelStatement]
type Name = String
-- | Things with names.  We don't include function "pointers" here
-- because pointerness is determined by use: regular application ':@:'
-- uses pointers, whereas full application 'FullApp' uses
-- non-pointers.
--
-- Key difference from old design: fullapplication, not regular
-- application, is the special case.
data Named
   = Var Name -- ^ Regular variable, including functions.
   | Con Name -- ^ Constructor
   -- The rest are only relevant to FOL? Could use GADT tricks
   -- to enforce this.
   | Rec Name -- ^ Recursive version of a function
   | Proj Int Name -- ^ Projector for a term constructor.
   | Unroll Int Name -- ^ An unrolling of a function.  Used for
                     -- multiple unrolling support.
   -- There is no 'Full' because full application is
   -- determined by context.
   deriving (Eq,Ord,Show,Data,Typeable)

data Expression
   = Named Named
   -- Regular application: f x y => f @ x @ y
   | Expression :@: Expression
   -- Full application: f x y => f(x,y).
   | FullApp Named [Expression]
   deriving (Show,Eq,Ord,Data,Typeable)

data TopLevelStatement
   = ContSat ContSat
   | Def Definition
   | DataType DataType
   | Import [String]
   deriving (Eq,Show,Ord,Data,Typeable)

-- No contracts for constructors! So, Name, not Named here.
data ContSat = Satisfies Name Contract
               deriving (Show,Eq,Ord,Data,Typeable)
               
-- | Expressions that might include 'case' matching.
data Case
   = Base Expression
   -- ^ 
   -- Case e pces@[(p1,ce1),...,(pk,cek)]
   -- ==> 
   -- case e of p1 -> ce1 ; ... ; pk -> cek
   | Case Expression [(Pattern,Case)]
   deriving (Show,Eq,Ord,Data,Typeable)

data Definition = Let Name [Name] Case
                  deriving (Show,Eq,Ord,Data,Typeable)
                  
data DataType = Data Name [(Name,Int,Contract)] -- Data constructors + arity + contract
                deriving (Eq,Show,Ord,Data,Typeable)

data Contract = Arr (Maybe Name) Contract Contract
                    -- ^ 'x : c1 -> c2', with 'x' optional.
                    | Pred Name Expression  -- {x:e}
                    | And Contract Contract
                    | Or  Contract Contract
                    | CF
                    | Any -- XXX: 'Any' is just '{x:True}', yeah?
                    deriving (Show,Eq,Ord,Data,Typeable)
