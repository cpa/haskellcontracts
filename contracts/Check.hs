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
  res <- sequence $ go order prog []
  return $ and res
  where go [] prog checkedDefs = []
        go (fs:fss) prog checkedDefs = check prog fs checkedDefs : go fss prog (fs++checkedDefs)
  
check :: Program -> [Variable] -> [Variable] -> IO Bool
check prog [] _ = error "Should not happen"
check prog [f] checkedDefs = do
  let hasBeenChecked checkedDefs (Def (Let f _ _)) = f `elem` checkedDefs
      hasBeenChecked checkedDefs (Def (LetCase f _ _ _)) = f `elem` checkedDefs
      hasBeenChecked _ _ = True
      safeSubset prog checkedDefs = filter (hasBeenChecked (f:checkedDefs)) prog
      tptpTheory = trans (safeSubset prog checkedDefs) f >>= simplify >>= toTPTP
      tmpFile = "tmp.tptp"
  putStrLn $ "Checking " ++ f ++ "..."
  writeFile tmpFile tptpTheory
  res <- isUnsat . last . lines <$> readProcess "ulimit -t 5 ; ./equinox" [tmpFile] ""
  removeFile tmpFile
  return res
    where isUnsat s = "Unsatisfiable" `elem` tails s
  
check prog _ _ = error "Not implemented yet"