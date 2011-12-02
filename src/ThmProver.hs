module ThmProver (module ThmProver, module Types.ThmProver) where

import Data.List (isInfixOf)
import Text.PrettyPrint.HughesPJ (render, vcat)

--import Haskell as H
import FOL as F
import Types.ThmProver

fof :: Theory
fof = Theory {
        showFormula = render . vcat . map F.toTPTP
      , header = const ""
      , fileExtension = "tptp"
      , footer = ""
      }
smt2 :: Theory
smt2 = Theory {
           showFormula = concatMap (F.toSMTLIB . F.unlabel)
         , header = F.showDefsSMTLIB
         , fileExtension = "smt2"
         , footer = "(check-sat)"
         }

provers :: [(ThmProver, ThmProverConf)]
provers = [ (Equinox, ThmProverConf
                        "equinox"
                        []
                        ("Unsatisfiable" `isInfixOf`)
                        fof
            )
          , (SPASS, ThmProverConf
                      "SPASS"
                      ["-PProblem=0","-PGiven=0","-TPTP"]
                      ("Proof found" `isInfixOf`)
                      fof
            )
          -- I can't locate any vampire usage docs, and '-h' and
          -- '--help' don't work :P From the CASC competition page I
          -- found that '-t <time>' can be used to time limit vampire.
          -- Experience shows that, by default, vampire has a 60
          -- second time limit.
          , (Vampire32, ThmProverConf
                          "vampire_lin32"
                          ["--mode", "casc" ,"--input_file"]
                          ("SZS status Unsatisfiable" `isInfixOf`)
                          fof
            )
          , (Vampire64, ThmProverConf
                          "vampire_lin64"
                          ["--mode", "casc" ,"--input_file"]
                          ("SZS status Unsatisfiable" `isInfixOf`)
                          fof
            )
          , (E, ThmProverConf
                  "eprover"
                  ["--tstp-format","-s"]
                  ("Unsatisfiable" `isInfixOf`)
                  fof
            )
          , (Z3, ThmProverConf
                   "z3"
                   ["-nw","-smt2"]
                   ("unsat" `isInfixOf`)
                   smt2
            )
          ]
