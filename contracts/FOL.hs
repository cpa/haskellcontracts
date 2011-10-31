{-# LANGUAGE DeriveFunctor #-}

module FOL (module FOL, module Haskell) where

--import qualified Haskell as H
import Haskell (Name,Named,MetaNamed(..),Expression,MetaExpression(..),Arity,appifyExpr,getName)
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
toTPTP f = header ++ "\n" ++ go [] f ++ "\n" ++ footer
  -- XXX, MAYBE TODO: add better header or comments. Right now the first "axiom"
  -- below is the name of the axiom.  The TPTP format also allows optional 4th and
  -- 5th fields for comments.  Would be nice to see each formula labeled with its
  -- type or source, to make debugging the generated .tptp file easier.
  --
  -- There is a TPTP package on hackage
  -- http://hackage.haskell.org/package/logic-TPTP-0.3.0.0.  It's very
  -- light on documentation, but the source might be worth a look.  It
  -- handles comments/annotations.  Curious to see if it handles
  -- quoting and case conversion automatically.
  where header = "fof(axiom,axiom,"
        footer = ").\n"
        -- 'go qs f' converts formula 'f' to TPTP syntax, assuming
        -- 'qs' are the names of the quantified variables in 'f'.
        go qs (Forall xs f) = "! " ++ goQList xs
                              ++ "  : (" ++ go (xs++qs) f ++ ")"
        go qs (f1 :=>: f2) = "(" ++ go qs f1 ++ ") => (" ++ go qs f2 ++ ")"
        go qs (f1 :<=>: f2) = "(" ++ go qs f1 ++ ") <=> (" ++ go qs f2 ++ ")"
        go qs (Not (t1 :=: t2)) =  goTerm qs t1 ++ " != " ++ goTerm qs t2
        go qs (Not f) = "~(" ++ go qs f ++ ")"
        go qs (Or []) = error "add suport for empty OR becomes false"
        go qs (Or fs) = "(" ++ (intercalate " | " (map (go qs) fs)) ++ ")"
        go qs (And []) = "$true"
        go qs (And fs) = "(" ++ (intercalate " & " (map (go qs) fs)) ++ ")"
        go qs Top = "$true"
        go qs Bottom = "$false"
        go qs (t1 :=: t2) = goTerm qs t1 ++ " = " ++ goTerm qs t2
        go qs (t1 :/=: t2) = goTerm qs t1 ++ " != " ++ goTerm qs t2
        go qs (CF t) = "cf(" ++ goTerm qs t ++ ")"

        goTerm qs (Named n) = goNamed qs n
        goTerm qs (e1 :@: e2) = "app(" ++ goTerm qs e1 ++ "," ++ goTerm qs e2 ++ ")"
        goTerm qs (FullApp f []) = goNamed qs f
        goTerm qs (FullApp f as) = goFull f ++ "("
                                   ++ (intercalate "," $ map (goTerm qs) as) ++ ")"

        goNamed qs (Var v) = goVar qs v
        goNamed qs (Con v) = "'" ++ v ++ "'"
        goNamed qs (Rec v) = v ++ "__R"
        goNamed qs (Proj i v) = "'"++v++"__"++show i++"'"

        -- Uppercase a list of quantified variables.
        goQList xs = "["++intercalate "," (map uppercase xs)++"]"
        -- Annotate a full application.  We only fully applied defined
        -- functions, and defined functions are never quantified over,
        -- so no 'qs' here.
        goFull = goNamed [] . fmap ("f__"++)
        -- Uppercase a variable if quantified.
        goVar qs v = if v `elem` qs then uppercase v else v
        uppercase = map toUpper

-- takes formulas and a list of arities for each definition
-- and returns those formulas using "full application" wherever possible
appifyF :: [Arity] -> [Formula] -> [Formula]
appifyF a fs = map (fmap $ appifyExpr a) fs
