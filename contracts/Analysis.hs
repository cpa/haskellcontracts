module Analysis where

import Data.Graph hiding (scc)
import Data.Graph.SCC 
import Data.List (nub)
import Haskell

graphFromProgram :: [DefGeneral Expression] -> (Graph,Vertex -> (Variable, Variable, [Variable]),Variable -> Maybe Vertex)
graphFromProgram p = graphFromEdges [ go d | d <- p, case d of Def _ -> True ; _ -> False ]
  where go (Def (Let f xs e)) = (f,f,freeVars f (xs++dataV) e)
        go (Def (LetCase f xs e pes)) = (f,f,concatMap (\(p,e) -> freeVars f (xs++dataV++p) e) $ (undefined,e):pes)
        dataV = dataVars =<< [ d | d <- p, case d of DataType _ -> True ; _ -> False ]
        
dataVars :: DefGeneral t -> [Variable]
dataVars (DataType (Data _ vacs)) = map fst3 vacs
  where fst3 (a,_,_) = a

freeVars :: Variable -> [Variable] -> Expression -> [Variable]
freeVars f xs BAD = []
freeVars f xs (Var v) = if  v `elem` xs then [] else [v]
freeVars f xs (App e1 e2) = freeVars f xs e1 ++ freeVars f xs e2
freeVars f xs (FullApp g es) = g : ((freeVars f xs) =<< es)
freeVars f xs (Sat _ _) = undefined -- yet
freeVars f xs (CF e) = freeVars f xs e

checkOrder :: [DefGeneral Expression] -> [[Variable]]
checkOrder p = reverse $ map nub topOrder
  where (g,a,b) = graphFromProgram p
        (e,f) = scc $ g
        topOrderAux = map (\(b,as) -> (map a as)) e
        topOrder = map (map (\(_,f,fs) -> f)) topOrderAux
        
