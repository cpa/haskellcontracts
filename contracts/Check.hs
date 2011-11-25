{-# LANGUAGE ParallelListComp #-}
module Main where

import Control.Exception (assert)
import Control.Applicative
import Control.Monad (when,unless)
import Data.List (tails,intersperse,intercalate,nub,intersect)
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import System.IO (hFlush,stdout)
import System.Exit (exitWith,ExitCode (ExitFailure))
import System.Process (system,readProcess,rawSystem,readProcessWithExitCode)
import System.Directory (removeFile,doesFileExist)
import System.FilePath (takeFileName,joinPath,(</>),(<.>))
import System.Posix.Process (exitImmediately)
import System.Console.CmdArgs.Verbosity

import Analysis (orderedChecks)
import Parser hiding (main)
import Translation (trans)
import Haskell
import FOL (toTPTP,simplify)
import ThmProver
import Options (Conf(..),getOpts)

-- XXX: Might want to put 'cfg' in a 'State'.
main = do
  cfg <- getOpts
  let f = file cfg
  when (ghci cfg) $ runGHCi cfg f
  when (type_check cfg) $ tc cfg f
  res <- checkFile cfg f
  if res
    then do
    whenNormal $ unless (dry_run cfg) $
      putStrLn $ f ++ ": all the contracts hold."
    else do
    whenNormal $ 
      putStrLn $ "There's at least one contract in " ++ f ++ " that doesn't hold."
    exitWith $ ExitFailure 1
 where
  args cfg file = [ "-fno-warn-unrecognized-pragma"
                  , "-XNoImplicitPrelude"
                  , idirs' cfg
                  , file ]
  idirs' cfg = "-i"++intercalate ":" (idirs cfg)

  run cmd args = exitImmediately =<< rawSystem cmd args

  runGHCi cfg f = run "ghci" (args cfg f)
  -- Pretty hackish: concatenate the "compiled" contracts to the end
  -- of the source file containing the "raw" contracts, and then ask
  -- GHC to typecheck the result.
  tc cfg f = do
    haskell <- readFile f
    let defs = parse haskell
    let contracts = [ c | ContSat (Satisfies _ c) <- defs ]
    let contracts' = map contract2Haskell contracts
    let haskell' = unlines [ "__contract__"++show i++" = "++s
                           | s <- contracts'
                           | i <- [(1::Int)..] ]
    let tempFile = f++".tc.hs" -- GHC complains if the input file doesn't end in ".hs" :P
    writeFile tempFile $
      unlines [ haskell
              , ""
              , "{- ##### CONTRACTS ##### -}"
              , ""
              , haskell'
              -- GHC wants a 'main' when I don't use '-c', and '-c'
              -- caused other problems.
              , "main = main"]
    let args' = "-fno-code" : args cfg tempFile
    whenLoud $
      putStrLn $ "Running:\n  ghc "++intercalate " " args'
    (code,out,err) <- readProcessWithExitCode "ghc" args' ""
    whenLoud $
      putStr $ out
    whenNormal $
      putStr $ err
    unless (keep_tmps cfg) $
      removeFile tempFile
    exitImmediately code

-- | Load a source file, recursively loading imports.
loadFile :: Conf -> FilePath -> IO Program
loadFile cfg f = do
  defs <- parse <$> readFile f
  concat <$> mapM recLoadFile defs
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
      prover = fromJust $ lookup (engine cfg) provers
      thy = theory prover
      -- Add contracts to the 'checks' and 'deps'.
      checks' = checks ++ (getContracts prog =<< checks)
      deps' = deps ++ (getContracts prog =<< deps)
      -- Check the minimality of 'checks' and 'deps'.
      out = assert (checks' == nub checks')
          $ assert (deps' == nub deps')
          $ assert (null (checks' `intersect` deps'))
          $ trans checks' deps'
            >>= simplify cfg >>= showFormula thy
      tmpFile = takeFileName f ++
                "." ++ intercalate "-" fs ++
                "." ++ fileExtension thy
  whenNormal $ do
    putStrLn $ "Writing " ++ tmpFile
    putStrLn $ show fs ++ " are mutually recursive. Checking them altogether..."
  writeFile tmpFile (unlines [header thy defs,out,footer thy])
  hFlush stdout
  res <- if not $ dry_run cfg
    then do 
    out <- readProcess (path prover) (opts prover ++ [tmpFile]) ""
    let res = unsat prover out
    whenNormal $ do
      whenLoud $ putStrLn out
      putStrLn $ if res then "OK :)" else "Not OK :("
    return res
    else return True
  unless (keep_tmps cfg) $
    removeFile tmpFile
  return res
 where
  fs = [def2Name d | d@(Def _) <- checks]
  contracts         = map (getContracts prog) checks
  multipleContracts = filter ((>1) . length) contracts
