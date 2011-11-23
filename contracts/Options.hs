{-# OPTIONS_GHC -fno-cse #-} -- For CmdArgs. See
-- http://hackage.haskell.org/packages/archive/cmdargs/latest/doc/html/System-Console-CmdArgs-Implicit.html
{-# LANGUAGE DeriveDataTypeable #-}
module Options where

import System.Console.CmdArgs
import Data.List (intercalate)

import ThmProver

data Conf = Conf { print_TPTP   :: Bool
                 , only_check   :: [String] 
                 , dry_run      :: Bool 
                 , engine       :: ThmProver
                 , idirs        :: [FilePath] -- "Include" directories
                 , type_check   :: Bool -- 'True' if we just want to run ghci
                 , file         :: FilePath
                 } deriving (Show, Data, Typeable)

getOpts = cmdArgs $ Conf 
  { print_TPTP = def
    &= help "Write first-order theories to FILE.<f1>-...-<fn>.tptp for each mutually dependent set {<f1>,...<fn>} of functions in FILE. These are the files given as input to the theorem prover (E.g. Equinox)."

  -- XXX: this mode is disabled.
  --
  -- To reenable this "check a single function" functionality, find
  -- the '(checks,defs)' s.t. 'f' in 'checks', and then either:
  --
  -- 1. run the checker on that pair, '(checks, defs)'.  This is the
  -- behavior described below.
  --
  -- 2. run checker on '[(f, defs++checks - [f])]'. This is an even
  -- less-sound version, where even contracts on mutually defined
  -- functions are assumed.
  , only_check = def
    &= ignore -- Don't actually display this option.
    &= typ "FUN"
    &= help "Limit checking to the functions FUN specified. This includes the contract for FUN, and the contracts of all functions that are mutually recursive with FUN. All other relevant, but non-mutual, contracts are assumed.  The default is to check all contracts in FILE."

  , dry_run = def
    &= help "Print the order in which contracts would be checked, but don't actually check them. If used in conjunction with '-p', the .tptp files will still be created."

    -- Interesting: The lower case versions of the names still work
    -- ??? E.g. you can say '--engine equinox'.
  , engine = Equinox -- The default option.
    &= typ "PROVER"
    &= help ("Use the specified theorem prover. The default is Equinox. The available provers are "++(intercalate ", " $ map show [(minBound::ThmProver)..maxBound])
++". The prover configurations are in ThmProver.hs.")

  , idirs = ["."] -- Always check current dir first.
    &= explicit -- Don't guess names for this option
    &= name "idir" &= name "i"
    &= typDir
    &= help "Add DIR to the paths searched for imports. This option may be specified multiple times. Import dirs are searched in the order specified, with an implicit \".\" (current dir) first."

  , type_check = def
    &= help "Load FILE into GHCi, instead of checking its contracts. Useful to typecheck and to run functions. If you only want to typecheck, than add support for 'ghc -e <dummy>'."

  , file = def
    &= argPos 0
    &= typFile
  } &= 
  verbosity &=
  program "Check" &=
  helpArg [explicit, name "h", name "help"] &= -- Avoid stupid '-?' default.
  summary "Haskell Contract Checker, V 1/0" &=
  details
  -- XXX, MAYBE TODO: support module names on command line, in addition to paths.
  [ "Default behaviour is '--engine equinox'. The FILE should be a path,"
  , "e.g. Foo/Bar/Baz.hs, not a module name, e.g. Foo.Bar.Baz.  GHC accepts"
  , "both, so maybe we should too?" ]

-- XXX, TODO: add switches that modify theory generation. See

main = print =<< getOpts
