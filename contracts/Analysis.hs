module Analysis where

import Data.Graph hiding (scc)
import Data.Graph.SCC (scc)
import Data.Maybe (fromJust)
import Data.List (nub)
import Haskell
import Parser


graphFromProgram :: Program -> (Graph,Vertex -> (Variable, Variable, [Variable]),Variable -> Maybe Vertex)
graphFromProgram p = graphFromEdges [ go d | d <- p, case d of Def _ -> True ; _ -> False ]
  where go (Def (Let f xs e)) = (f,f,freeVars f (xs++dataV) e ++ varsInCont p f)
        go (Def (LetCase f xs e pes)) = (f,f,varsInCont p f ++ (concatMap (\(p,e) -> freeVars f (xs++dataV++p) e) $ (undefined,e):pes))
--        go (ContSat (Satisfies f c))  = (f,f,contVars f dataV c)
        dataV = dataVars =<< [ d | d <- p, case d of DataType _ -> True ; _ -> False ]

varsInCont p f = 
  if all (==Nothing) contracts
  then []
  else contVars f [] (fromJust $ head $ filter (/= Nothing) contracts)
    where contracts = map (\d -> case d of ContSat (Satisfies x c) -> if x == f then Just c else Nothing ; _ -> Nothing) p
-- TODO Here I assume there's only one contract per function.

contVars f xs (AppC x c1 c2) = contVars f xs c1 ++ contVars f xs c2
contVars f xs (Pred x e)     = freeVars f (x:xs) e
contVars f xs (And c1 c2)    = contVars f xs c1 ++ contVars f xs c2
contVars f xs (Or c1 c2)     = contVars f xs c1 ++ contVars f xs c2
contVars f xs Any            = []
        
dataVars :: DefGeneral -> [Variable]
dataVars (DataType (Data _ vacs)) = map fst3 vacs
  where fst3 (a,_,_) = a

freeVars :: Variable -> [Variable] -> Expression -> [Variable]
freeVars f xs BAD = []
freeVars f xs (Var v) = if  v `elem` xs then [] else [v]
freeVars f xs (App e1 e2) = freeVars f xs e1 ++ freeVars f xs e2
freeVars f xs (FullApp g es) = g : ((freeVars f xs) =<< es)

checkOrder :: Program -> [[Variable]]
checkOrder p = reverse $ map nub topOrder
  where (g,a,b) = graphFromProgram p
        (e,f) = scc $ g
        topOrderAux = map (\(b,as) -> (map a as)) e
        topOrder = map (map (\(_,f,fs) -> f)) topOrderAux
