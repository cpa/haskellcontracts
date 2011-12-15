{-# OPTIONS_GHC -fno-cse #-} -- For CmdArgs. See
-- http://hackage.haskell.org/packages/archive/cmdargs/latest/doc/html/System-Console-CmdArgs-Implicit.html
{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
module Options where

import System.Console.CmdArgs
import Data.List (intercalate)

import Types.ThmProver

showAll :: forall a. (Bounded a, Enum a, Show a) => a -> String
showAll _ = intercalate ", " $ map show [(minBound::a)..maxBound]

getOpts = cmdArgs $ Conf 
  { keep_tmps = def
    &= help "Keep temporary files. For contract checking, this means write first-order theories to FILE.<f1>-...-<fn>.tptp for each mutually dependent set {<f1>,...<fn>} of functions in FILE. These are the files given as input to the theorem prover (E.g. Equinox).  For type checking, this means write FILE.tc.hs containing FILE and the compiled versions of FILE's contracts."

  -- There are at least two ways to define the "check a single
  -- function 'f'" functionality. First, find the '(checks,defs)'
  -- s.t. 'f' in 'checks', and then either:
  --
  -- 1. run the checker on that pair, '(checks, defs)'.  This is the
  -- implemented behaviour.
  --
  -- 2. run checker on '[(f, defs++checks - [f])]'. This is an even
  -- less-sound version, where even contracts on mutually defined
  -- functions are assumed.
  , only_check = def
--    &= ignore -- Don't actually display this option.
    &= typ "FUN"
    &= help "Limit checking to the functions FUN specified. This includes the contract for FUN, and the contracts of all functions that are mutually recursive with FUN. All other relevant, but non-mutual, contracts are assumed.  The default, when no FUNs are specified this way, is to check all contracts in FILE.  This option may be given more than once, to specify multiple functions to check."

  , dry_run = def
    &= help "Print the order in which contracts would be checked, but don't actually check them. If used in conjunction with '-p', the .tptp files will still be created."

    -- Interesting: The lower case versions of the names still work
    -- ??? E.g. you can say '--engine equinox'.
  , engine = Equinox -- The default option.
    &= typ "PROVER"
    &= help ("Use the specified theorem prover. The default is Equinox. The available provers are "++showAll Equinox
++". The prover configurations are in ThmProver.hs.")

  , idirs = ["."] -- Always check current dir first.
    &= explicit -- Don't guess switch names for this option
    &= name "idir" &= name "i"
    &= typDir
    &= help "Add DIR to the paths searched for imports. This option may be specified multiple times. Import dirs are searched in the order specified, with an implicit \".\" (current dir) first."

  , file = def
    &= argPos 0
    &= typFile

  , ghci = def
    &= help "Load FILE, and the compilation of FILE's contracts, into GHCi.  Causes GHCi to type-check the functions and contracts, and allows you to run functions.  Use '-k' to keep the temp file with the compiled contracts, or use ':!cat -n <temp file>' in GHCi to examine the temp file."

  , type_check = def
    &= help "Type-check FILE, and the compilation of FILE's contracts, using GHC. Use '-k' to keep the temp file with the compiled contracts."

  , no_min = def
    &= help "Don't use the 'min' predicate in the translation. This should degrade performance, but makes the resulting theory file much easier to read, and can be used to debug changes to 'min' placement, e.g. to check if they are too restrictive."

  , no_ptr = def
    &= help "Don't generate any pointer axioms.  This is a poor-mans approximation to Koen's suggestion to only generate the necessary pointer axioms.  NB: IT'S THE USER'S PROBLEM TO ENSURE THERE ARE NO 'app'S."

  , case_qs = Project
    &= explicit
    &= name "case-qs"
    &= help ("How many quantifiers to use in case-expression translation.  The choices are "++showAll Project++". Default is Project. Hybrid means use quantifiers for standard branches, and projections for the UNR branch.")

  , case_implies = def
    &= explicit
    &= name "case-implies"
    &= help "Use \"old\"-style case-expression translation which conjoins implications.  The default is to disjoin conjunctions."

  , unrolls = def
    &= help "Specify the number of additional unrollings to perform when translating function definitions."

  } &= 
  verbosity &=
  program "hcc" &=
  helpArg [explicit, name "h", name "help"] &= -- Avoid stupid '-?' default switch.
  summary "Haskell Contract Checker, V 1/0" &=
  details
  -- XXX, MAYBE TODO: support module names on command line, in addition to paths.
  [ "Default behaviour is '--engine equinox'. The FILE should be a path,"
  , "e.g. Foo/Bar/Baz.hs, not a module name, e.g. Foo.Bar.Baz.  GHC accepts"
  , "both, so maybe we should too?" ]

-- XXX, TODO: add switches that modify theory generation. See

main = print =<< getOpts
