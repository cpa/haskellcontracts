{-# LANGUAGE DeriveFunctor #-}

module FOL where

import qualified Haskell as H
import Debug.Trace
import Data.Char (toUpper)
import Data.List (intercalate)

type Term = MetaTerm Variable
type Formula = MetaFormula Term

data Variable = Regular String
              | BAD
              | UNR
              deriving (Eq)

instance Show Variable where
  show (Regular v) = v
  show BAD = "bad"
  show UNR = "unr"

-- XXX, ???: Is there any reason we don't just use
-- Haskell.MetaExpression ? That would simplify things (e.g. appifyF
-- could use appifyExpr instead of duplicating it).
data MetaTerm a = Var a
                | App [MetaTerm a]
                | FullApp a [MetaTerm a]
                deriving (Eq,Functor)

instance Show a => Show (MetaTerm a) where
  show (Var v) = show v
  show (App []) = error "Cannot apply nothing"
  show (App [t]) = show t
  show (App ts) = "app(" ++ show (App (init ts)) ++ "," ++ show (last ts) ++ ")"
  show (FullApp f []) = show f
  show (FullApp f as) = show f ++ "(" ++ (intercalate "," $ map show as) ++ ")"


infix 7 :<=>:
infix 7 :=>:
data MetaFormula a = Forall [a] (MetaFormula a)
                   | (MetaFormula a) :=>: (MetaFormula a)
                   | (MetaFormula a) :<=>: (MetaFormula a)
                   | Not (MetaFormula a)
                   | Or [MetaFormula a]
                   | And [MetaFormula a]
                   | Top
                   | Bottom
                   | a :=: a
                   | a :/=: a
                   | CF a
                   deriving (Show,Eq,Functor)


-- forall a . x && y --> (forall a . x) && (forall a . y)
splitOnAnd :: Formula -> [Formula]
splitOnAnd (Forall xs (And fs)) = map (Forall xs) fs
splitOnAnd (Forall xs f) = map (Forall xs) $ splitOnAnd f
splitOnAnd (And fs) = concatMap splitOnAnd fs
splitOnAnd f = [f]

appifyFOF a f = fmap (\x -> case x of App ((Var x):xs) -> case lookup x a of
                                        Just n -> if n == length xs then FullApp x xs else App (Var x : xs)
                                        Nothing -> App (Var x : xs)
                                      x -> x) f


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
        go (Forall xs f) = "! " ++ show xs ++ "  : (" ++ go f ++ ")"
        go (f1 :=>: f2) = "(" ++ go f1 ++ ") => (" ++ go f2 ++ ")"
        go (f1 :<=>: f2) = "(" ++ go f1 ++ ") <=> (" ++ go f2 ++ ")"
        go (Not (t1 :=: t2)) =  goTerm t1 ++ " != " ++ goTerm t2
        go (Not f) = "~(" ++ go f ++ ")"
        go (Or fs) = "(" ++ (intercalate " | " (map go fs)) ++ ")"
        go (And fs) = "(" ++ (intercalate " & " (map go fs)) ++ ")"
        go Top = "$true"
        go Bottom = "$false"
        go (t1 :=: t2) = goTerm t1 ++ " = " ++ goTerm t2
        go (t1 :/=: t2) = goTerm t1 ++ " != " ++ goTerm t2
        go (CF t) = "cf(" ++ goTerm t ++ ")"
        goTerm (Var v) = show v
        goTerm (App []) = error "Cannot apply nothing"
        goTerm (App [t]) = goTerm t
        goTerm (App ts) = "app(" ++ goTerm (App (init ts)) ++ "," ++ goTerm (last ts) ++ ")"
        goTerm (FullApp f []) = show f
        goTerm (FullApp f as) = show f ++ "(" ++ (intercalate "," $ map show as) ++ ")"

-- takes formulas and a list of arities for each definition
-- and returns those formulas using "full application" wherever possible
appifyF :: [H.Arity] -> [Formula] -> [Formula]
appifyF a fs = map (fmap go) fs
  -- XXX, REFACTOR: copied from Haskell.hs appifyExpr.  This is a
  -- separate function because the Haskell and FOL use different
  -- expression types, but this seems completely unnecessary.
  where go :: Term -> Term
        go (App (App es1 : es2)) = go (App (es1++es2))
        go (App (Var (Regular v) : es)) = case H.lookupT (H.Var v) a of
          Just n -> if length es' == n
                    then FullApp (Regular v) es'
                    else App ((Var $ Regular $ H.makePtr v) : es')
          Nothing -> App ((Var $ Regular v) : es')
          where es' = map go es
        go (App es) = App $ map go es
        go (FullApp v es) = FullApp v $ map go es
        go e@(Var (Regular v)) = case H.lookupT (H.Var v) a of
          Just 0 -> e
          Just n -> Var $ Regular $ H.makePtr v -- XXX, ??? BUG: Won't this make 'f_ptr_ptr' ???
          Nothing -> e
        go e@(Var v) = e
