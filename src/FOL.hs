module FOL (module FOL, module FOLTypes) where

import Debug.Trace
import Data.Char (toUpper)
import Data.List (intercalate)
import Control.Monad.State (gets)
import Text.PrettyPrint.HughesPJ
import Data.Generics (mkT,everywhere)

import qualified Haskell as H
import Haskell (appifyExpr,getName)
import Options (Conf(no_min))
import FOLTypes
import TranslationTypes

unlabel (LabeledFormula _ e) = e

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

--trivializeMin :: Formula -> Formula
trivializeMin = everywhere (mkT unMin) where
-- t where
  unMin :: Formula -> Formula -- XXX: unnec with single level type?
  unMin (Min t) = Top
  unMin f       = f

-- | Replace e.g. 'And(fs1++[And fs]++fs2)' with 'And(fs1++fs++fs2)'.
--flattenNAryOps :: Formula -> Formula
flattenNAryOps = everywhere (mkT go) where
  go :: Formula -> Formula -- XXX: unnec with single level type?
  go (And fs) = And $ concat $ map go' fs where
    go' (And fs) = fs
    go' f        = [f]
  go (Or  fs) = Or  $ concat $ map go' fs where
    go' (Or  fs) = fs
    go' f        = [f]
  go f = f

--removeConstants :: Formula -> Formula
removeConstants = everywhere (mkT go) where
  go :: Formula -> Formula -- XXX: unnec with single level type?
  go (Forall [] f) = f
  go (Exists [] f) = f
  -- assuming a non-empty domain here.
  go fm@(Forall xs f) = if f `elem` [Top,Bottom] then f else fm
  go fm@(Exists xs f) = if f `elem` [Top,Bottom] then f else fm
  go fm@(f1 :=>: f2)  = case (f1,f2) of
                       (Top, f)    -> f
                       (f, Top)    -> Top
                       (Bottom, f) -> Top
                       (f, Bottom) -> Not f
                       _           -> fm
  go fm@(f1 :<=>: f2) = case (f1,f2) of
                       (Top, f)    -> f
                       (f, Top)    -> f
                       (Bottom, f) -> Not f
                       (f, Bottom) -> Not f
                       _           -> fm
  go (Or fs) = if any (==Top) fs then Top else f
    where fs' = filter (/=Bottom) fs
          f = if null fs' then Bottom else Or fs'
  go (And fs) = if any (==Bottom) fs then Bottom else f
    where fs' = filter (/=Top) fs
          f = if null fs' then Top else And fs'
  go f = f


simplify :: Conf -> LabeledFormula -> [LabeledFormula]
simplify cfg = splitOnAndLabeled
             . flattenNAryOps
             . removeConstants
             . maybeTrivializeMin
 where
  maybeTrivializeMin = if no_min cfg then trivializeMin else id


toTPTP :: LabeledFormula -> Doc
toTPTP (LabeledFormula l f) = fof
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
  where fof = text "fof"
              <> (parens $ vcat $ punctuate comma [text l,text "axiom",body]) 
              <> text "."
        body = go [] f

        -- n-ary infix operator 'op' with recursive pretty printer
        -- 'rec' and arguments 'fs'.  'n = length fs'.
        nary rec qs op fs = parens $ sep $ prepunctuate op (map (rec qs) fs)
        -- specialized to textual operators
        naryText rec qs t fs = nary rec qs (text t) fs
        naryF = naryText go
        naryT = naryText goTerm
        -- n-ary function application
        fun qs f args = f <> nary goTerm qs comma args

        quantifier qs q xs f = parens $ hang (text q <+> goQList xs <+> text ":")
                                             2 (go (xs++qs) f)

        prepunctuate sep [d] = [d]
        prepunctuate sep (d:ds) = d : map (sep <+>) ds
        -- 'go qs f' converts formula 'f' to TPTP syntax, assuming
        -- 'qs' are the names of the quantified variables in 'f'.
        go qs (Forall xs f) = quantifier qs "!" xs f
        go qs (Exists xs f) = quantifier qs "?" xs f

        go qs (f1 :=>: f2) = naryF qs "=>" [f1,f2]
        go qs (f1 :<=>: f2) = naryF qs "<=>" [f1,f2]
        go qs (Not (t1 :=: t2)) = go qs (t1 :/=: t2)
        go qs (t1 :=: t2) = naryT qs "=" [t1,t2]
        go qs (t1 :/=: t2) = naryT qs "!=" [t1,t2]

        go qs (Not f) = text "~" <> parens (go qs f)

        go qs (Or fs) = naryF qs "|" fs
        go qs (And fs) = naryF qs "&" fs

        go qs Top = text "$true"
        go qs Bottom = text "$false"

        go qs (CF t) = fun qs (text "cf") [t]
        go qs (Min t) = fun qs (text "$min") [t]

        goTerm qs (Named n) = goNamed qs n
        goTerm qs (e1 :@: e2) = fun qs (text "app") [e1,e2]
        goTerm qs (FullApp f []) = goNamed qs f
        goTerm qs (FullApp f as) = fun qs (goFull f) as

        goNamed qs (Var v) = goVar qs v
        goNamed qs (Con v) = text $ "c_" ++ v -- "'" ++ v ++ "'"
        goNamed qs (Rec v) = text $ v ++ "__R"
        goNamed qs (Proj i v) = text $ "'"++v++"__"++show i++"'"

        -- Uppercase a list of quantified variables.
        goQList xs = brackets $ hsep $ punctuate comma $ map (text . uppercase) xs
        -- Annotate a full application.  We only fully applied defined
        -- functions, and defined functions are never quantified over,
        -- so no 'qs' here.
        goFull = goNamed [] . fmap ("f__"++)
        -- Uppercase a variable if quantified.
        goVar qs v = text $ if v `elem` qs then uppercase v else v
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
appify :: [Arity] -> LabeledFormula -> LabeledFormula
appify a = fmap (fmap $ appifyExpr a)

appifyF :: Formula -> Fresh Formula
appifyF f = do
  a <- gets arities
  return $ fmap (appifyExpr a) f
