module Main where

import Analysis (checkOrder)
import Parser hiding (main)
import Translation (trans)
import Haskell
import FOL (toTPTP,simplify,removeWeakAnnotations)
import ThmProver

import Control.Monad (when,unless)
import Data.List (tails,intersperse)
import System.Environment (getArgs)
import System.IO (hFlush,stdout)
import System.Exit (exitWith,ExitCode (ExitFailure))
import System.Process (system,readProcess)
import System.Directory (removeFile)
import Control.Applicative

data Conf = Conf { timeLimit :: Int
                 , printTPTP :: Bool
                 , toCheck   :: [String] 
                 , dryRun    :: Bool 
                 , engine    :: String
                 , noWeak    :: Bool
                 , quiet     :: Bool
                 }

conf flags = go flags (Conf 10 False [] False "equinox" True False)
  where go ("-t":n:flags)       cfg = go flags (cfg {timeLimit=read n :: Int})
        go ("-p":flags)         cfg = go flags (cfg {printTPTP=True})
        go ("-c":f:flags)       cfg = go flags (cfg {toCheck=f:(toCheck cfg)})
        go ("--dry-run":flags)  cfg = go flags (cfg {dryRun=True})
        go ("--engine":e:flags) cfg = go flags (cfg {engine=e})
        go ("--weak":flags)     cfg = go flags (cfg {noWeak=False})
        go ("-q":flags)         cfg = go flags (cfg {quiet=True})
        go (f:flags)            cfg = error $ f ++": unrecognized option"
        go []                   cfg = cfg

main = do
  f:flags <- getArgs
  let cfg = conf flags
  res <- checkFile f cfg
  if res
    then unless (dryRun cfg || quiet cfg) $ putStrLn $ f ++ ": all the contracts hold."
    else do
    unless (quiet cfg) $ 
      putStrLn $ "There's at least one contract in " ++ f ++ " that took more than " ++ show (timeLimit cfg) ++ " sec to prove."
    exitWith $ ExitFailure 1

checkFile :: String -> Conf -> IO Bool  
checkFile f cfg = do
  s <- readFile f
  let prog = haskell $ lexer s
      order = checkOrder prog
      cfg' = if toCheck cfg == [] then cfg {toCheck = concat order} else cfg
  system $ "ulimit -t " ++ show (timeLimit cfg)
  unless (quiet cfg) $ 
    putStrLn $ "Time limit for each contract is: " ++ show (timeLimit cfg) ++ " sec. WARNING: ulimit may or may not work on your box..."
  res <- sequence $ go prog [] cfg' order
  return $ and res
    where go prog checkedDefs cfg [] = []
          go prog checkedDefs cfg (fs:fss) = if (any (`elem` (toCheck cfg)) fs) 
                                             then check prog fs cfg checkedDefs : go prog (fs++checkedDefs) cfg fss
                                             else go prog (fs++checkedDefs) cfg fss


hasBeenChecked :: [Variable] -> DefGeneral -> Bool
hasBeenChecked checkedDefs (Def (Let f _ _)) = f `elem` checkedDefs
hasBeenChecked checkedDefs (Def (LetCase f _ _ _)) = f `elem` checkedDefs
hasBeenChecked checkedDefs (ContSat (Satisfies f _)) = f `elem` checkedDefs
hasBeenChecked _ _  = True

hasNoContract :: Variable -> [DefGeneral] -> Bool
hasNoContract f prog = and $ (flip map) prog $ \d -> case d of
  ContSat (Satisfies g _) -> f /= g
  _ -> True


-- the distinction between one or several functions to check at the same time is 
-- not stricly necessary but it gives a nicer output to the user
check :: Program -> [Variable] -> Conf -> [Variable] -> IO Bool
check prog [] cfg _ = error "There should be at least one definition!"
check prog [f] cfg checkedDefs | f `hasNoContract` prog = return True
                               | otherwise = do
  let safeSubset prog checkedDefs = filter (hasBeenChecked (f:checkedDefs)) prog
      tptpTheory = (if (noWeak cfg) 
                    then map (fmap removeWeakAnnotations) $ trans (safeSubset prog checkedDefs) [f] 
                    else trans (safeSubset prog checkedDefs) [f])
                   >>= simplify >>= toTPTP
      tmpFile = "tmp.tptp"
  when (printTPTP cfg) $ do
    writeFile (f++".tptp") tptpTheory
    unless (quiet cfg) $
      putStrLn $ "Writing " ++ f ++ ".tptp"
  unless (quiet cfg) $
    putStr $ "Checking " ++ f ++ "..."
  hFlush stdout
  if not $ dryRun cfg  
    then do 
    writeFile tmpFile tptpTheory
    let (enginePath,engineOpts,engineUnsat) = case lookup (engine cfg) provers of
          Nothing -> error "Engine not recognized. Supported engines are: equinox, SPASS, vampire and E"
          Just x  -> (path x,opts x,unsat x)
    res <- engineUnsat <$> readProcess  enginePath (engineOpts ++ [tmpFile]) ""
    removeFile tmpFile
    when res $ 
      unless (quiet cfg) $ 
        putStrLn "\tOK!"
    return res
    else do
    unless (quiet cfg) $ 
      putStrLn ""
    return True
  
check prog fs cfg checkedDefs | all (`hasNoContract` prog) fs = return True
                              | otherwise = do
  let safeSubset prog checkedDefs = filter (hasBeenChecked (fs++checkedDefs)) prog
      tptpTheory = (if (noWeak cfg) 
                    then map (fmap removeWeakAnnotations) $ trans (safeSubset prog checkedDefs) fs
                    else trans (safeSubset prog checkedDefs) fs)
                   >>= simplify >>= toTPTP
      tmpFile = "tmp.tptp"
  when (printTPTP cfg) $ do
    writeFile (head fs ++ ".tptp") tptpTheory
    unless (quiet cfg) $
      putStrLn $ "Writing " ++ (head fs) ++ ".tptp"
  unless (quiet cfg) $
    putStr $ showfs fs ++ "are mutually recursive. Checking them altogether..."
  hFlush stdout
  if not $ dryRun cfg  
    then do 
    writeFile tmpFile tptpTheory
    let (enginePath,engineOpts,engineUnsat) = case lookup (engine cfg) provers of
          Nothing -> error "Engine not recognized. Supported engines are: equinox, SPASS, vampire"
          Just x  -> (path x,opts x,unsat x)
    res <- engineUnsat <$> readProcess  enginePath (engineOpts ++ [tmpFile]) ""
    removeFile tmpFile
    when res $
      unless (quiet cfg) $
        putStrLn "\tOK!"
    return res
    else do
    unless (quiet cfg) $
      putStrLn ""
    return True
    where showfs fs = (concat $ intersperse " " fs) ++ " "
