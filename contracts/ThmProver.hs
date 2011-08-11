module ThmProver where

import Data.List

data ThmProverOpt = T { path :: FilePath
                      , opts :: [String]
                      , unsat  :: String -> Bool }

type ThmProver = (String,ThmProverOpt)
                    
provers :: [ThmProver]
provers = [ ("equinox", T "equinox" [] ("Unsatisfiable" `isInfixOf`))
          , ("SPASS", T "SPASS" ["-PProblem=0","-PGiven=0","-TPTP"] ("Proof found" `isInfixOf`))
          , ("vampire", T "vampire_lin64" ["--mode", "casc" ,"--input_file"] ("Refutation" `isInfixOf`)) ]