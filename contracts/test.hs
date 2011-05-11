module Main where
import Parser
import Translation
import Control.Monad
import FOL
import System.Environment


fulltest f = do
  s <- readFile f
  forM_ (map toTPTP $ (haskell $ lexer s) >>= trans) putStrLn
  
main = do
  f:_ <- getArgs
  fulltest f
  