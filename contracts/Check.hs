module Main where

import Analysis (checkOrder)
import Parser hiding (main)
import Translation
import Control.Monad
import FOL (toTPTP,simplify)
import Data.List (tails,intersperse)
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
        go []             cfg = cfg

main = do
  f:flags <- getArgs
  let cfg = conf flags
  res <- checkFile f cfg
  if res 
    then putStrLn $ f ++ ": all the contracts hold."
    else putStrLn $ "There's at least one contract in " ++ f ++ " that took more than " ++ show (timeLimit cfg) ++ " sec to prove."
  
checkFile f cfg = do
  s <- readFile f
  let prog = appify $ haskell $ lexer s
      order = checkOrder prog
  system $ "ulimit -t " ++ show (timeLimit cfg)
  putStrLn $ "Time limit for each contract is: " ++ show (timeLimit cfg) ++ " sec. WARNING: ulimit may or may not work on your box..."
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
  when (printTPTP cfg) $ do
    writeFile (f++".tptp") tptpTheory
    putStrLn $ "Writing " ++ f ++ ".tptp"
  putStr $ "Checking " ++ f ++ "..."
  hFlush stdout
  writeFile tmpFile tptpTheory
  res <- isUnsat . last . lines <$> readProcess "equinox" [tmpFile] ""
  removeFile tmpFile
  when res $ 
    putStrLn "\tOK!"
  return res
    where isUnsat s = "Unsatisfiable" `elem` tails s
  
check prog fs cfg checkedDefs = do
  let safeSubset prog checkedDefs = filter (hasBeenChecked (fs++checkedDefs)) prog
      tptpTheory = trans (safeSubset prog checkedDefs) fs >>= simplify >>= toTPTP
      tmpFile = "tmp.tptp"
  when (printTPTP cfg) $ do
    writeFile (head fs ++ ".tptp") tptpTheory
    putStrLn $ "Writing " ++ (head fs) ++ ".tptp"
  putStr $ showfs fs ++ "are mutually recursive. Checking them altogether..."
  hFlush stdout
  writeFile tmpFile tptpTheory
  res <- isUnsat . last . lines <$> readProcess "equinox" [tmpFile] ""
  removeFile tmpFile
  when res $
    putStrLn "\tOK!"
  return res
    where isUnsat s = "Unsatisfiable" `elem` tails s
          showfs fs = (concat $ intersperse " " fs) ++ " "
