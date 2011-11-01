module ThmProver where

import Data.List (isInfixOf)

import Haskell as H
import FOL as F

data ThmProverOpt = T { path :: FilePath
                      , opts :: [String]
                      , unsat  :: String -> Bool
                      , theory :: Theory
                      }

data Theory = Theory { 
      showFormula :: F.Formula -> String
    , header :: [H.DefGeneral] -> String
    , fileExtension :: String
    , footer :: String
    }

type ThmProver = (String,ThmProverOpt)

fof :: Theory
fof = Theory {
        showFormula = F.toTPTP
      , header = const ""
      , fileExtension = "tptp"
      , footer = ""
      }
smtlib :: Theory
smtlib = Theory {
           showFormula = F.toSMTLIB
         , header = F.showDefsSMTLIB
         , fileExtension = "smtlib"
         , footer = "(check-sat)"
         }
                    
provers :: [ThmProver]
provers = [ ("equinox", T "equinox"
                          []
                          ("Unsatisfiable" `isInfixOf`)
                          fof
            )
          , ("SPASS", T "SPASS"
                        ["-PProblem=0","-PGiven=0","-TPTP"]
                        ("Proof found" `isInfixOf`)
                        fof
            )
          , ("vampire", T "vampire_lin64"
                          ["--mode", "casc" ,"--input_file"]
                          ("Refutation" `isInfixOf`)
                          fof
            )
          , ("E", T "eprover"
                    ["--tstp-format","-s"]
                    ("Unsatisfiable" `isInfixOf`)
                    fof
            )
          , ("z3", T "z3"
                     ["-smt2"]
                     ("unsat" `isInfixOf`)
                     smtlib
            )
          ]
