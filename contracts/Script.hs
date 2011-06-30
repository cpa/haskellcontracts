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

main = do
  files:c:_ <- getArgs
  putStrLn $ files ++ " " ++ c
  cts <- toTheory files c
  writeFile (files ++ ".tptp") cts
  
test = toTheory "test2.hs" "add"