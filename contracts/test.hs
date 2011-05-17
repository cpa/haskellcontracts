module Main where

import Parser hiding (main)
import Translation
import Control.Monad
import FOL
import System.Environment


test f = do
  s <- readFile f
  forM_ (map toTPTP $ (haskell $ lexer s) >>= trans >>= simplify) putStrLn

test2 f = do
  s <- readFile f
  forM_ (map toTPTP $ (transExp $ haskell $ lexer s) >>= simplify) putStrLn



main = do
  f:_ <- getArgs
  test f
  