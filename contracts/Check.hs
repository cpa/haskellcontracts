module Main where

import Analysis (checkOrder)
import Parser hiding (main)
import Translation (trans)
import Haskell
import FOL (toTPTP,simplify)
import ThmProver

import Control.Monad (when,unless)
import Data.List (tails,intersperse)
import System.Environment (getArgs)
import System.IO (hFlush,stdout)
import System.Exit (exitWith,ExitCode (ExitFailure))
import System.Process (system,readProcess)
import System.Directory (removeFile)
import System.FilePath (takeFileName)
import Control.Applicative

data Conf = Conf { printTPTP :: Bool
                 , toCheck   :: [String] 
                 , dryRun    :: Bool 
                 , engine    :: String
                 , quiet     :: Bool
                 , verbose   :: Bool
                 }

conf flags = go flags defaults
  where go ("-p":flags)         cfg = go flags (cfg {printTPTP=True})
        go ("-c":f:flags)       cfg = go flags (cfg {toCheck=f:(toCheck cfg)})
        go ("--dry-run":flags)  cfg = go flags (cfg {dryRun=True})
        go ("--engine":e:flags) cfg = go flags (cfg {engine=e})
        go ("-q":flags)         cfg = go flags (cfg {quiet=True})
        go ("-v":flags)         cfg = go flags (cfg {verbose=True})
        go (f:flags)            cfg = error $ f ++": unrecognized option"
        go []                   cfg = cfg

        defaults = Conf {printTPTP=False, toCheck=[], dryRun=False,
                         engine="equinox", quiet=False, verbose=False}
usage = unlines
  [ "usage: ./Check file [-p] [-q] [-c f] [--dry-run] [--engine equinox|vampire|SPASS|E]"
  , ""
  , "Default behaviour is: ./Check file --engine equinox"
  , ""
  , " * -p means that the first-order TPTP theory is written in files for each contract proof (the name of the file is outputed on stdout)"
  , " * -q outputs nothing, not even the result (I use it to make time measurements less painful)"
  , " * -c f means that only the contract for f (and the contracts of functions that are mutually recursive with f, if any) will be checked, assuming every other contract. If there is no -c option, all the contracts in the file will be checked."
  , " * --dry-run just prints the order in which contracts would be checked but doesn't check anything. If used in conjunction with -p it'll still write then tptp files."
  , " * --engine equinox|vampire|SPASS|E let you choose the automated theorem prover to use as backend. More provers can be easily added in ThmProver.hs"
  ]

main = do
  ffs@(f:flags) <- getArgs
  when ("-h" `elem` ffs || "--help" `elem` ffs) usageAndExit
  let cfg = conf flags
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

checkFile :: String -> Conf -> IO Bool  
checkFile f cfg = do
  s <- readFile f
  let prog = haskell $ lexer s
      order = checkOrder prog
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

hasNoContract :: Variable -> [DefGeneral] -> Bool
hasNoContract f prog = not $ f `elem` [g | ContSat (Satisfies g _) <- prog]

check :: FilePath -> Program -> [Variable] -> Conf -> [Variable] -> IO Bool
check f prog [] cfg _ = error "There should be at least one definition!"
check f prog fs cfg checkedDefs | all (`hasNoContract` prog) fs = return True
                                | otherwise = do
  let safeSubset prog checkedDefs = filter (hasBeenChecked (fs++checkedDefs)) prog
      tptpTheory = (trans (safeSubset prog checkedDefs) fs)
                   >>= simplify >>= toTPTP
      tmpFile = (takeFileName f) ++ "." ++ head fs ++ ".tptp"
  unless (quiet cfg) $ do
    putStrLn $ "Writing " ++ tmpFile
    putStrLn $ show fs ++ " are mutually recursive. Checking them altogether..."
  writeFile tmpFile tptpTheory
  hFlush stdout
  res <- if not $ dryRun cfg
    then do 
    let (enginePath,engineOpts,engineUnsat) = case lookup (engine cfg) provers of
          Nothing -> error "Engine not recognized. Supported engines are: equinox, SPASS, vampire, E"
          Just x  -> (path x,opts x,unsat x)
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
