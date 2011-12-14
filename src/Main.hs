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
import Options(getOpts)


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
  -- GHC/GHCi args
  args cfg file = [ "-fno-warn-unrecognised-pragmas"
                  , "-XNoImplicitPrelude"
                  , idirs' cfg
                  , file ]
  idirs' cfg = "-i"++intercalate ":" (idirs cfg)

  -- Load 'f' and the compilation of 'f's contracts into GHCi.
  runGHCi cfg f = do
    tempFile <- makeTCFile cfg f
    code <- rawSystem "ghci" (args cfg tempFile)
    unless (keep_tmps cfg) $
      removeFile tempFile
    exitImmediately code

  -- Typecheck 'f' and the compilation of 'f's contracts with GHC.
  tc cfg f = do
    tempFile <- makeTCFile cfg f
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

  -- Pretty hackish: concatenate the "compiled" contracts to the end
  -- of the source file containing the "raw" contracts.  Returns the
  -- name of the tmp file.
  makeTCFile cfg f = do
    haskell <- readFile f
    let defs = parse haskell
    -- XXX: more helpful to put the contract in the comment too.
    -- However, we don't have a pretty printer, and we don't retain
    -- the input src in the contract AST.
    let haskell' = unlines [ "-- "++show e++"\n"
                             ++"__contract__"++show i++" = "++contract2Haskell c
                           | ContSat (Satisfies e c) <- defs
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
    return tempFile

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
  -- could be elided (e.g. 'ptrAxiom').
  let prog' = appify (arities prog) prog
      sccs = orderedChecks prog'
      -- If the set of functions to check is restricted, then check
      -- only the SCCs containing one of the specified functions.
      fs = only_check cfg
      sccs' = if null fs
              then sccs
              -- The SCCs are '(checks,deps)', so we look for 'fs' in
              -- the 'checks'.
              else filter (any (`elem` fs) . map tls2Name . fst) sccs
  whenNormal $ unless (null fs) $
    putStrLn $ "Only checkings SCCs for "++show fs
  and <$> mapM (check cfg f prog') sccs'

getContracts :: [TopLevelStatement] -> TopLevelStatement -> [TopLevelStatement]
getContracts prog d = [c | c@(ContSat (Satisfies g _)) <- prog, g == tls2Name d]

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
      checks' = nub $ checks ++ (getContracts prog =<< checks)
      deps'   = nub $ deps   ++ (getContracts prog =<< deps)

      -- DEBUG
      -- show' = show . map concise where
      --   concise d@(Def _) = "D "++tls2Name d
      --   concise t@(DataType _) = "T "++tls2Name t
      --   concise c@(ContSat _) = "C "++tls2Name c

          -- = (trace (unlines [ "checks': "++show' checks'
          --                   , "checks: "++show' checks
          --                   , "deps': "++show' deps'
          --                   , "deps: "++show' deps ])
          -- $ assert (checks == nub checks))

      -- Check the minimality of 'checks' and 'deps'.
      out = assert (checks == nub checks)
          $ assert (deps == nub deps)
          $ assert (null (checks `intersect` deps))
          $ assert (null (checks' `intersect` deps'))
          $ showFormula thy $ simplify cfg
                              =<< trans cfg checks' deps'
      tmpFile = takeFileName f ++
                "." ++ intercalate "-" fs ++
                "." ++ fileExtension thy
  whenNormal $ do
    putStrLn $ "Writing " ++ tmpFile
    putStrLn $ show fs ++ " are mutually recursive. Checking them altogether..."
  writeFile tmpFile (unlines [header thy cfg defs,out,footer thy])
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
  fs = [tls2Name d | d@(Def _) <- checks]
  contracts         = map (getContracts prog) checks
  multipleContracts = filter ((>1) . length) contracts
