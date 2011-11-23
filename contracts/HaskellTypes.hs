{-# LANGUAGE DeriveFunctor #-}

-- Types used in the Haskell module.  Moved here to avoid cyclic
-- dependencies.
module HaskellTypes where

type Arity = (Name,Int)

-- All this meta stuff is just here to allow me to derive functors for free.
type DataType = MetaDataType Expression
type Expression = MetaExpression Named
type DefGeneral = MetaDefGeneral Expression
type Program = [DefGeneral]
type Pattern = (Name, [Name])
type Contract = MetaContract Expression
type Definition = MetaDefinition Expression

type Name = String
-- | Things with names.  We don't include function "pointers" here
-- because pointerness is determined by use: regular application ':@:'
-- uses pointers, whereas full application 'FullApp' uses
-- non-pointers.
--
-- Key difference from old design: fullapplication, not regular
-- application, is the special case.
data MetaNamed a =
             Var a -- ^ Regular variable, including functions.
           | Con a -- ^ Constructor
           -- The rest are only relevant to FOL? Could use GADT tricks
           -- to enforce this.
           | Rec a -- ^ Recursive version of a function
           | Proj Int a -- ^ Projector for a term constructor.
           -- There is no 'Full' because full application is
           -- determined by context.
           deriving (Eq,Ord,Show,Functor)

type Named = MetaNamed Name

data MetaExpression v = Named v
                      -- Regular application: f x y => f @ x @ y
                      | (MetaExpression v) :@: (MetaExpression v)
                      -- Full application: f x y => f(x,y).
                      | FullApp v [MetaExpression v]
                      deriving (Show,Eq,Functor,Ord)

data MetaDefGeneral a = ContSat (MetaContSat a)
                      | Def (MetaDefinition a)
                      | DataType (MetaDataType a)
                      | Import [String]
                      deriving (Eq,Show,Functor,Ord)

-- No contracts for constructors! So, Name, not Named here.
data MetaContSat a = Satisfies Name (MetaContract a)
                   deriving (Show,Eq,Functor,Ord)
               
data MetaDefinition a = Let Name [Name] a
                      -- LetCase f xs e [(p_i,e_i)]_i ~ f xs = case e of [p_i -> e_i]_i
                      | LetCase Name [Name] a [(Pattern,a)]
                      deriving (Show,Eq,Functor,Ord)
                  
data MetaDataType a = Data Name [(Name,Int,MetaContract a)] -- Data constructors + arity + contract
                    deriving (Eq,Show,Functor,Ord)

data MetaContract a = Arr (Maybe Name) (MetaContract a) (MetaContract a)
                    -- ^ 'x : c1 -> c2', with 'x' optional.
                    | Pred Name a  -- {x:e}
                    | And (MetaContract a) (MetaContract a)
                    | Or  (MetaContract a) (MetaContract a)
                    | CF
                    | Any -- XXX: 'Any' is just '{x:True}', yeah?
                    deriving (Show,Eq,Functor,Ord)

