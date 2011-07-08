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

data Conf = Conf { timeLimit :: Int
                 , printTPTP :: Bool } 

conf flags = go flags (Conf 10 False)
  where go ("-t":n:flags) cfg = go flags (cfg {timeLimit=read n :: Int})
        go ("-p":flags)   cfg = go flags (cfg {printTPTP=True})
        go (_:flags)      cfg = go flags cfg

main = do
  f:flags <- getArgs
  res <- checkFile f (conf flags)
  if res 
    then putStrLn $ f ++ ": all the contracts hold."
    else putStrLn $ "There's at least one contract in " ++ f ++ " that took more than 5 sec to prove."
  
checkFile f cfg = do
  s <- readFile f
  let prog = appify $ haskell $ lexer s
      order = checkOrder prog
  system $ "ulimit -t " ++ show (timeLimit cfg)
  res <- sequence $ go prog [] cfg order
  return $ and res
  where go prog checkedDefs cfg [] = []
        go prog checkedDefs cfg (fs:fss) = check prog fs cfg checkedDefs : go prog (fs++checkedDefs) cfg fss
  
hasBeenChecked checkedDefs (Def (Let f _ _)) = f `elem` checkedDefs
hasBeenChecked checkedDefs (Def (LetCase f _ _ _)) = f `elem` checkedDefs
hasBeenChecked _ _ = True

hasNoContract f prog = and $ (flip map) prog $ \d -> case d of
  ContSat (Satisfies g _) -> f /= g
  _ -> True

check :: Program -> [Variable] -> Conf -> [Variable] -> IO Bool
check prog [] cfg _ = error "There should be at least one definition!"
check prog [f] cfg checkedDefs | f `hasNoContract` prog = return True
                           | otherwise = do
  let safeSubset prog checkedDefs = filter (hasBeenChecked (f:checkedDefs)) prog
      tptpTheory = trans (safeSubset prog checkedDefs) [f] >>= simplify >>= toTPTP
      tmpFile = "tmp.tptp"
  putStr $ "Checking " ++ f ++ "..."
  writeFile tmpFile tptpTheory
  res <- isUnsat . last . lines <$> readProcess "./equinox" [tmpFile] ""
  removeFile tmpFile
  when res $ 
    putStrLn "\tOK!"
  when (printTPTP cfg) $ do
    putStrLn tptpTheory
    putStrLn "\n==============================================================\n"
  return res
    where isUnsat s = "Unsatisfiable" `elem` tails s
  
check prog fs cfg checkedDefs = do
  let safeSubset prog checkedDefs = filter (hasBeenChecked (fs++checkedDefs)) prog
      tptpTheory = trans (safeSubset prog checkedDefs) fs >>= simplify >>= toTPTP
      tmpFile = "tmp.tptp"
  putStr $ showfs fs ++ "are mutually recursive. Checking them altogether..."
  writeFile tmpFile tptpTheory
  res <- isUnsat . last . lines <$> readProcess "./equinox" [tmpFile] ""
  removeFile tmpFile
  when res $
    putStrLn "\tOK!"
  when (printTPTP cfg) $ do
    putStrLn tptpTheory
    putStrLn "\n==============================================================\n"
  return res
    where isUnsat s = "Unsatisfiable" `elem` tails s
          showfs [] = ""
          showfs (f:fs) = f ++ " " ++ showfs fs
