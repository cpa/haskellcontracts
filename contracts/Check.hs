module Main where

import Analysis (orderedChecks)
import Parser hiding (main)
import Translation (trans)
import Haskell
import FOL (toTPTP,simplify)
import ThmProver

import Control.Exception (assert)
import Control.Applicative
import Control.Monad (when,unless)
import Data.List (tails,intersperse,intercalate,nub,intersect)
import System.Environment (getArgs)
import System.IO (hFlush,stdout)
import System.Exit (exitWith,ExitCode (ExitFailure))
import System.Process (system,readProcess)
import System.Directory (removeFile,doesFileExist)
import System.FilePath (takeFileName,joinPath,(</>),(<.>))
import System.Posix.Process (executeFile)

data Conf = Conf { printTPTP :: Bool
                 , toCheck   :: [String] 
                 , dryRun    :: Bool 
                 , engine    :: String
                 , quiet     :: Bool
                 , verbose   :: Bool
                 , idirs     :: [FilePath] -- include directories
                 , typeCheck :: Bool -- True is we just want to run ghci
                 }

conf flags = go flags defaults
  where go ("-p":flags)         cfg = go flags (cfg {printTPTP=True})
        -- Assuming we search earlier idirs first, we add new dirs at
        -- the end.  XXX: not sure what GHC does here ... but easy to
        -- check.
        go ("-i":idir:flags)    cfg = go flags (cfg {idirs = idirs cfg ++ [idir]})
        -- To reenable this "check a single function" functionality:
        -- find the '(checks,defs)' s.t. 'f' in 'checks', then run
        -- checker on '[(f, defs++checks - [f])]'.
        go ("-c":f:flags)       cfg = error "-c FUN is not supported currently :("
                                   -- go flags (cfg {toCheck=f:(toCheck cfg)})
        go ("--dry-run":flags)  cfg = go flags (cfg {dryRun=True})
        go ("--engine":e:flags) cfg = go flags (cfg {engine=e})
        go ("-q":flags)         cfg = go flags (cfg {quiet=True})
        go ("-v":flags)         cfg = go flags (cfg {verbose=True})
        go ("-t":flags)         cfg = go flags (cfg {typeCheck = True})
        go (f:flags)            cfg = error $ f ++": unrecognized option"
        go []                   cfg = cfg

        defaults = Conf { printTPTP=False, toCheck=[], dryRun=False
                        , engine="equinox", quiet=False, verbose=False
                        , idirs=["."], typeCheck=False
                        }
usage = unlines
  [ "usage: ./Check FILE [-t] [-p] [-q] [-c FUN] [i DIR] [--dry-run] [--engine (equinox|vampire32|vampire64|SPASS|E|z3]"
  , ""
  , " * -t"
  , "        run 'ghci' on the FILE. Useful to typecheck and to run functions."
  , "        If you only want to typecheck, than add support for 'ghc -e <dummy>'."
  , " * -p"
  , "        write the first-order TPTP theory is written in files for each"
  , "        contract proof (the name of the file is outputed on stdout)"
  , " * -i DIR"
  , "        add DIR to paths searched for imports. Import dirs are searched"
  , "        in the order specified, with an implicit \".\" (current dir) first."
  , " * -q"
  , "        output nothing, not even the result (I use it to make time measurements"
  , "        less painful)"
  , " * -c FUN"
  , "        check only the contract for FUN (and the contracts of functions"
  , "        that are mutually recursive with FUN, if any), assuming"
  , "        every other contract. If there is no -c option, all the contracts in"
  , "        the file will be checked."
  , " * --dry-run"
  , "        prints the order in which contracts would be checked but doesn't"
  , "        check anything. If used in conjunction with -p it'll still write"
  , "        then tptp files."
  , " * --engine (equinox|vampire32|vampire64|SPASS|E|z3)"
  , "        choose the automated theorem prover to use as backend. More provers can"
  , "        be easily added in ThmProver.hs"
  , ""
  -- XXX, MAYBE TODO: support module names on command line, in addition to paths.
  , "NB: most options must come *after* the file name :P"
  , ""
  , "Default behaviour is '--engine equinox'.  The FILE should be a path,"
  , "e.g. Foo/Bar/Baz.hs, not a module name, e.g. Foo.Bar.Baz.  GHC accepts"
  , "both, so maybe we should too?"
  ]

main = do
  ffs@(f:flags) <- getArgs
  when ("-h" `elem` ffs || "--help" `elem` ffs) usageAndExit
  let cfg = conf flags
  when (typeCheck cfg) $ runGHCi f cfg
  res <- checkFile cfg f
  if res
    then unless (dryRun cfg || quiet cfg) $ putStrLn $ f ++ ": all the contracts hold."
    else do
    unless (quiet cfg) $ 
      putStrLn $ "There's at least one contract in " ++ f ++ " that doesn't hold."
    exitWith $ ExitFailure 1
 where
  usageAndExit = do
    putStrLn usage
    exitWith $ ExitFailure 1
  runGHCi f cfg = executeFile "ghci" usePATH args env
   where usePATH = True
         env = Nothing
         args = ["-XNoImplicitPrelude", idirs', f]
         idirs' = "-i"++intercalate ":" (idirs cfg)

-- | Load a source file, recursively loading imports.
loadFile :: Conf -> FilePath -> IO Program
loadFile cfg f = do
  s <- readFile f
  let p = haskell $ lexer s
  concat <$> mapM recLoadFile p
 where
  -- Expand imports to a list of their declarations.
  --
  -- XXX: If we want to avoid importing the same module more than
  -- once, then we could track the list of imported modules in 'cfg'.
  recLoadFile (Import mod) = findAndLoad mod (idirs cfg)
  recLoadFile d            = return [d]

  findAndLoad mod [] = error $ "Could not locate module "
                       ++(show $ intercalate "." mod)
                       ++" relative to include directories "
                       ++show (idirs cfg)
                       ++" when loading file "++f
  findAndLoad mod (i:is) = do
                   cond <- doesFileExist f'
                   if cond
                   then loadFile cfg f'
                   else findAndLoad mod is
   where f' = i </> joinPath mod <.> "hs"

checkFile :: Conf -> FilePath -> IO Bool
checkFile cfg f = do
  prog <- loadFile cfg f
  -- XXX: Appification is an optional optimization, so we could add a
  -- switch to control it.  However, to turn it off we also need to
  -- change how most formulas are generated, since most formulas are
  -- generated using full application where possible. If appification
  -- were disabled, the formulas relating full and partial application
  -- could be elided (e.g. 'dPtr').
  let prog' = appify (arities prog) prog
  and <$> mapM (check cfg f prog') (orderedChecks prog')

getContracts :: [DefGeneral] -> DefGeneral -> [DefGeneral]
getContracts prog d = [c | c@(ContSat (Satisfies g _)) <- prog, g == def2Name d]

check :: Conf -> FilePath -> Program
      -> (Program,Program) -- ^ '(checks, deps)': assume 'deps' and check 'checks'
      -> IO Bool
check cfg f prog (checks,deps) | all null contracts = return True
                               | otherwise          = do
  let
      defs = filter defOrType (checks++deps) where
        defOrType (Def _)      = True
        defOrType (DataType _) = True
        defOfType _            = False
      (enginePath,engineOpts,engineUnsat,thy) =
        case lookup (engine cfg) provers of
          Nothing -> error "Engine not recognized. Try -h."
          Just x  -> (path x,opts x,unsat x,theory x)
      -- Add contracts to the 'checks' and 'deps'.
      checks' = checks ++ (getContracts prog =<< checks)
      deps' = deps ++ (getContracts prog =<< deps)
      -- Check the minimality of 'checks' and 'deps'.
      out = assert (checks' == nub checks')
          $ assert (deps' == nub deps')
          $ assert (null (checks' `intersect` deps'))
          $ trans checks' deps'
            >>= simplify >>= showFormula thy
      tmpFile = takeFileName f ++
                "." ++ intercalate "-" fs ++
                "." ++ fileExtension thy
  unless (quiet cfg) $ do
    putStrLn $ "Writing " ++ tmpFile
    putStrLn $ show fs ++ " are mutually recursive. Checking them altogether..."
  writeFile tmpFile (unlines [header thy defs,out,footer thy])
  hFlush stdout
  res <- if not $ dryRun cfg
    then do 
    out <- readProcess enginePath (engineOpts ++ [tmpFile]) ""
    let res = engineUnsat out
    unless (quiet cfg) $ do
      when (verbose cfg) $ putStrLn out
      putStrLn $ if res then "OK :)" else "Not OK :("
    return res
    else return True
  unless (printTPTP cfg) $
    removeFile tmpFile
  return res
 where
  fs = [def2Name d | d@(Def _) <- checks]
  contracts         = map (getContracts prog) checks
  multipleContracts = filter ((>1) . length) contracts
