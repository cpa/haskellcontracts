module Main where

import Parser hiding (main)
import Translation
import Control.Monad
import FOL
import System.Environment
import System.IO


toTheory f c = do
  s <- readFile f
  return $ trans (haskell $ lexer s) c >>= simplify >>= toTPTP
blo f c = do
  s <- readFile f
  return $ (haskell $ lexer s) 

main = do
  files:c:_ <- getArgs
  putStrLn $ files ++ " " ++ c
  cts <- toTheory files c
  writeFile (files ++ ".tptp") cts
  
test = toTheory "test.hs" "gt"
