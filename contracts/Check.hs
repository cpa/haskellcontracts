module Main where

import Analysis (checkOrder)
import Parser hiding (main)
import Translation
import Control.Monad
import FOL (toTPTP,simplify)
import Data.List (tails)
import System.Environment
import System.IO
import Haskell
import System.Process
import System.Directory (removeFile)
import Control.Applicative

main = do
  f:_ <- getArgs
  res <- checkFile f
  if res 
    then putStrLn $ f ++ ": all the contracts hold."
    else putStrLn $ "There's at least one contract in " ++ f ++ " that took more than 5 sec to prove."
  
checkFile f = do
  s <- readFile f
  let prog = appify $ haskell $ lexer s
      order = checkOrder prog
  system "ulimit -t 5"
  res <- sequence $ go order prog []
  return $ and res
  where go [] prog checkedDefs = []
        go (fs:fss) prog checkedDefs = check prog fs checkedDefs : go fss prog (fs++checkedDefs)
  
hasBeenChecked checkedDefs (Def (Let f _ _)) = f `elem` checkedDefs
hasBeenChecked checkedDefs (Def (LetCase f _ _ _)) = f `elem` checkedDefs
hasBeenChecked _ _ = True

check :: Program -> [Variable] -> [Variable] -> IO Bool
check prog [] _ = error "Should not happen"
check prog [f] checkedDefs = do
  let safeSubset prog checkedDefs = filter (hasBeenChecked (f:checkedDefs)) prog
      tptpTheory = trans (safeSubset prog checkedDefs) [f] >>= simplify >>= toTPTP
      tmpFile = "tmp.tptp"
  putStr $ "Checking " ++ f ++ "..."
  writeFile tmpFile tptpTheory
  res <- isUnsat . last . lines <$> readProcess "./equinox" [tmpFile] ""
  removeFile tmpFile
  when res $ 
    putStrLn "\tOK!"
  return res
    where isUnsat s = "Unsatisfiable" `elem` tails s
  
check prog fs checkedDefs = do
  let safeSubset prog checkedDefs = filter (hasBeenChecked (fs++checkedDefs)) prog
      tptpTheory = trans (safeSubset prog checkedDefs) fs >>= simplify >>= toTPTP
      tmpFile = "tmp.tptp"
  putStr $ showfs fs ++ "are mutually recursive. Checking them altogether..."
  writeFile tmpFile tptpTheory
  res <- isUnsat . last . lines <$> readProcess "./equinox" [tmpFile] ""
  removeFile tmpFile
  when res $
    putStrLn "\tOK!"
  return res
    where isUnsat s = "Unsatisfiable" `elem` tails s
          showfs [] = ""
          showfs (f:fs) = f ++ " " ++ showfs fs
