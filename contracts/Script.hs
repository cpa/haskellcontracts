module Main where

import Parser hiding (main)
import Translation
import Control.Monad
import FOL
import System.Environment
import System.IO

test f = do
  s <- readFile f
  forM_ (map toTPTP $ (transExp $ haskell $ lexer s) >>= simplify) putStrLn

toTheory f = do
  s <- readFile f
  return $ concatMap toTPTP $ (transExp $ haskell $ lexer s) >>= simplify

main = do
  files <- getArgs
  forM_ files $ \f -> do
    cts <- toTheory f
    writeFile (f++".tptp") cts
  