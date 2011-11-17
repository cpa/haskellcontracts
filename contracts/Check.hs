module Main where

import Analysis (checkOrder)
import Parser hiding (main)
import Translation (trans)
import Haskell
import FOL (toTPTP,simplify)
import ThmProver

import Control.Monad (when,unless)
import Data.List (tails,intersperse,intercalate)
import System.Environment (getArgs)
import System.IO (hFlush,stdout)
import System.Exit (exitWith,ExitCode (ExitFailure))
import System.Process (system,readProcess)
import System.Directory (removeFile,doesFileExist)
import System.FilePath (takeFileName,joinPath,(</>),(<.>))
import System.Posix.Process (executeFile)
import Control.Applicative

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
  res <- checkFile f cfg
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
loadFile :: FilePath -> Conf -> IO Program
loadFile f cfg = do
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
                   then loadFile f' cfg
                   else findAndLoad mod is
   where f' = i </> joinPath mod <.> "hs"

checkFile :: String -> Conf -> IO Bool  
checkFile f cfg = do
  prog <- loadFile f cfg
  let order = checkOrder prog
      cfg' = if toCheck cfg == [] then cfg {toCheck = concat order} else cfg
  res <- sequence $ go prog [] cfg' order
  return $ and res
    where go prog checkedDefs cfg [] = []
          go prog checkedDefs cfg (fs:fss) = if (any (`elem` (toCheck cfg)) fs) 
                                             then check f prog fs cfg checkedDefs : go prog (fs++checkedDefs) cfg fss
                                             else go prog (fs++checkedDefs) cfg fss


hasBeenChecked :: [Variable] -> DefGeneral -> Bool
hasBeenChecked checkedDefs (Def (Let f _ _)) = f `elem` checkedDefs
hasBeenChecked checkedDefs (Def (LetCase f _ _ _)) = f `elem` checkedDefs
hasBeenChecked checkedDefs (ContSat (Satisfies f _)) = f `elem` checkedDefs
hasBeenChecked _ _  = True

getContracts :: [DefGeneral] -> Variable -> [DefGeneral]
getContracts prog f = [c | c@(ContSat (Satisfies g _)) <- prog, g == f]

check :: FilePath -> Program -> [Variable] -> Conf -> [Variable] -> IO Bool
check f prog [] cfg _ = error "There should be at least one definition!"
check f prog fs cfg checkedDefs | all null contracts           = return True
                                | not (null multipleContracts) = error $
          -- XXX, TODO: this is easy to fix: just conjoin all
          -- contracts for each function.
          "Some functions have more than one contract,"
          ++"but we only support one contract currently."
          ++"Namely: "++show multipleContracts
                                | otherwise = do
  let safeSubset prog checkedDefs = filter (hasBeenChecked (fs++checkedDefs)) prog

      (enginePath,engineOpts,engineUnsat,thy) =
        case lookup (engine cfg) provers of
          Nothing -> error "Engine not recognized. Try -h."
          Just x  -> (path x,opts x,unsat x,theory x)

      defs = safeSubset prog checkedDefs
      out = trans defs fs
            >>= simplify >>= showFormula thy
      tmpFile = takeFileName f ++ "." ++ head fs ++ "." ++ fileExtension thy
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
  contracts         = map (getContracts prog) fs
  multipleContracts = filter ((>1) . length) contracts
