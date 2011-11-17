{-# LANGUAGE DeriveFunctor #-}

module FOL (module FOL, module Haskell) where

import qualified Haskell as H
import Haskell (Name,Named,MetaNamed(..),Expression,MetaExpression(..),Arity,appifyExpr,getName)
import Debug.Trace
import Data.Char (toUpper)
import Data.List (intercalate)

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

-- forall a . x && y --> (forall a . x) && (forall a . y)

splitOnAndLabeled :: LabeledFormula -> [LabeledFormula]
splitOnAndLabeled (LabeledFormula lbl f)
  = [LabeledFormula (lbl++show i) f' | (i,f') <- zip [1..] (splitOnAnd f)]

splitOnAnd :: Formula -> [Formula]
splitOnAnd (Forall xs (And fs)) = map (Forall xs) fs
splitOnAnd (Forall xs f) = map (Forall xs) $ splitOnAnd f
splitOnAnd (And fs) = concatMap splitOnAnd fs
splitOnAnd Top = []
splitOnAnd f = [f]

trivializeMin :: Formula -> Formula
trivializeMin = t where
  t f = case f of
    Forall xs f -> Forall xs (t f)
    f1 :=>: f2  -> t f1 :=>: t f2
    f1 :<=>: f2 -> t f1 :<=>: t f2
    Not f       -> Not $ t f
    Or fs       -> Or $ map t fs
    And fs      -> And $ map t fs
    Min t       -> Top
    f           -> f

removeConstants :: Formula -> Formula
removeConstants (Forall [] f) = removeConstants f
removeConstants (Forall xs f) = if f' == Top then Top else Forall xs f'
  where f' = removeConstants f
removeConstants (f1 :=>: f2) = case (removeConstants f1, removeConstants f2) of
                                 (Top, f)    -> f
                                 (Bottom, f) -> Top
                                 (f, Top)    -> Top
                                 (f, Bottom) -> Bottom
                                 (f1', f2')  -> f1' :=>: f2'
removeConstants (f1 :<=>: f2) = case (removeConstants f1, removeConstants f2) of
                                  (Top, f)    -> f
                                  (f, Top)    -> f
                                  (Bottom, f) -> Not f
                                  (f, Bottom) -> Not f
                                  (f1', f2')  -> f1' :<=>: f2'
removeConstants (Not f) = Not $ removeConstants f
removeConstants (Or []) = Bottom
removeConstants (And []) = Top
removeConstants (Or fs) = if any (==Top) fs' then Top else f
    where fs' = map removeConstants fs
          fs'' = filter (/=Bottom) fs'
          f = if null fs'' then Bottom else Or fs''
removeConstants (And fs) = if any (==Bottom) fs' then Bottom else f
    where fs' = map removeConstants fs
          fs'' = filter (/=Top) fs'
          f = if null fs'' then Top else And fs''
removeConstants f = f




simplify :: LabeledFormula -> [LabeledFormula]
simplify = splitOnAndLabeled . fmap removeConstants -- . trivializeMin


toTPTP :: LabeledFormula -> String
toTPTP (LabeledFormula l f) = header ++ "\n" ++ go [] f ++ "\n" ++ footer
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
  where header = "fof("++l++",axiom,"
        footer = ").\n"
        -- 'go qs f' converts formula 'f' to TPTP syntax, assuming
        -- 'qs' are the names of the quantified variables in 'f'.
        go qs (Forall xs f) = "(! " ++ goQList xs
                              ++ " : "++go (xs++qs) f ++ ")"
        go qs (f1 :=>: f2) = "(" ++ go qs f1 ++ " => " ++ go qs f2 ++ ")"
        go qs (f1 :<=>: f2) = "(" ++ go qs f1 ++ " <=> " ++ go qs f2 ++ ")"
        go qs (Not (t1 :=: t2)) =  "("++goTerm qs t1 ++ " != " ++ goTerm qs t2++")"
        go qs (Not f) = "~(" ++ go qs f ++ ")"
        go qs (Or fs) = "(" ++ (intercalate " | " (map (go qs) fs)) ++ ")"
        go qs (And fs) = "(" ++ (intercalate " & " (map (go qs) fs)) ++ ")"
        go qs Top = "$true"
        go qs Bottom = "$false"
        go qs (t1 :=: t2) = "("++goTerm qs t1 ++ " = " ++ goTerm qs t2++")"
        go qs (t1 :/=: t2) = "("++goTerm qs t1 ++ " != " ++ goTerm qs t2++")"
        go qs (CF t) = "cf(" ++ goTerm qs t ++ ")"
        go qs (Min t) = "min("++goTerm qs t++")"

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

toSMTLIB :: Formula -> String
toSMTLIB f = header ++ "\n" ++ go f ++ "\n" ++ footer
  where header = "(assert "
        footer = ")\n"
        -- 'go f' converts formula 'f' to TPTP syntax, assuming
        -- 'qs' are the names of the quantified variables in 'f'.
        go (Forall xs f) = "(forall " ++ goQList xs++" "
                              ++ go f ++ ")"
        go (f1 :=>: f2) = "(=> " ++ go f1 ++" "++ go f2 ++ ")"
        go (f1 :<=>: f2) = "(= " ++ go f1 ++" "++ go f2 ++ ")"
        go (Not f) = "(not " ++ go f ++ ")"
        go (Or fs) = "(or " ++ (intercalate " " (map (go) fs)) ++ ")"
        go (And fs) = "(and " ++ (intercalate " " (map (go) fs)) ++ ")"
        go Top = "true" -- XXX, ???: are these right?
        go Bottom = "false"
        go (t1 :=: t2) = "(= "++goTerm t1 ++ " " ++ goTerm t2++")"
        go (t1 :/=: t2) = go (Not (t1 :=: t2))
        go (CF t) = "(cf " ++ goTerm t ++ ")"
        go (Min t) = "(min "++goTerm t++")"

        goTerm (Named n) = goNamed n
        goTerm (e1 :@: e2) = "(app " ++ goTerm e1 ++ " " ++ goTerm e2 ++ ")"
        goTerm (FullApp f []) = goNamed f
        goTerm (FullApp f as) = "("++goFull f++" "
                                   ++ (intercalate " " $ map (goTerm) as) ++ ")"

        goNamed (Var v) = v
        goNamed (Con v) = v
        goNamed (Rec v) = v ++ "__R"
        goNamed (Proj i v) = v++"__"++show i

        goQList xs = "("++intercalate " " (map annote xs)++")"
        -- Annotate a full application.  We only fully applied defined
        -- functions, and defined functions are never quantified over,
        -- so no 'qs' here.
        goFull = goNamed . fmap ("f__"++)
        annote x = "("++x++" Real)"

showDefsSMTLIB defs = unlines $ cf:app:unr:bad:map showDef arities where
  arities = concat $ map expand $ H.arities defs
  expand (v,k) = if k == 0 then [(v,0),(v++"__R",0)] else vsFun++vsProj
    where
      -- XXX, FIX: these generate extra junk: recursive versions of
      -- constructors and projectors for non-constructor functions.
      -- This makes the comparison with Equinox unfair.
      --
      -- Checking the case of the first letter in v would be enough to
      -- remove redundancy here ... or better, actually pass the full
      -- list of defined names and their arities.
      vsFun = [ (v,0)
              , (v++"__R",0)
              , ("f__"++v,k)
              , ("f__"++v++"__R",k)
              ]
      vsProj = concat [ [(v++"__"++show i,0)
                        , ("f__"++v++"__"++show i,1)]
                      | i <- [1..k]]
  cf = "(declare-fun cf (Real) Bool)"
  min = "(declare-fun min (Real) Bool)"
  app = "(declare-fun app (Real Real) Real)"
  unr = "(declare-const UNR Real)"
  bad = "(declare-const BAD Real)"
  showDef (v,k) =
    if k == 0
    then "(declare-const "++v++" Real)"
    else "(declare-fun "++v++" ("++intercalate " " (replicate k "Real")++") Real)"

-- takes formulas and a list of arities for each definition
-- and returns those formulas using "full application" wherever possible
appifyF :: [Arity] -> LabeledFormula -> LabeledFormula
appifyF a = fmap (fmap $ appifyExpr a)
