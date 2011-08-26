module Analysis where

import Data.Graph hiding (scc)
import Data.Graph.SCC (scc)
import Data.Maybe (fromJust)
import Data.List (nub)
import Haskell
import Parser


-- takes a program and returns a graph where nodes are functions
-- and a is connected to b iff b's definition uses a
graphFromProgram :: Program -> (Graph,Vertex -> (Variable, Variable, [Variable]),Variable -> Maybe Vertex)
graphFromProgram p = graphFromEdges [ go d | d <- p, case d of Def _ -> True ; _ -> False ]
  where go (Def (Let f xs e)) = (f,f,freeVars (xs++dataV) e ++ varsInCont p f)
        go (Def (LetCase f xs e pes)) = (f,f,varsInCont p f ++ (concatMap (\(p,e) -> freeVars (xs++dataV++p) e) $ ([],e):pes))
--        go (ContSat (Satisfies f c))  = (f,f,contVars f dataV c)
        dataV = dataVars =<< [ d | d <- p, case d of DataType _ -> True ; _ -> False ]

-- finds the contract for f in p and returns the variables in it
-- Consider this example: 
-- f x = x
-- f ::: {x: eq x 0} -> {y: eq y 0}
-- f doesn't use any function so it has nothing pointing to it in the graph
-- but still, we need to include eq's definition in the theory!
varsInCont p f = 
  if all (==Nothing) contracts
  then []
  else contVars f [] (fromJust $ head $ filter (/= Nothing) contracts)
    where contracts = map (\d -> case d of ContSat (Satisfies x c) -> if x == f then Just c else Nothing ; _ -> Nothing) p
-- TODO Here I assume there's only one contract per function.

-- returns the variables used in a contract
contVars f xs (AppC x c1 c2) = contVars f xs c1 ++ contVars f xs c2
contVars f xs (Pred x e)     = freeVars (x:xs) e
contVars f xs (And c1 c2)    = contVars f xs c1 ++ contVars f xs c2
contVars f xs (Or c1 c2)     = contVars f xs c1 ++ contVars f xs c2
contVars f xs CF             = []
        
dataVars :: DefGeneral -> [Variable]
dataVars (DataType (Data _ vacs)) = map fst3 vacs
  where fst3 (a,_,_) = a

-- returns the list of free variables in an expression defining a function
-- xs is the list of arguments of the function
freeVars :: [Variable] -> Expression -> [Variable]
freeVars xs BAD = []
freeVars xs (Var v) = if  v `elem` xs then [] else [v]
freeVars xs (App e1 e2) = freeVars xs e1 ++ freeVars xs e2
freeVars xs (FullApp g es) = g : ((freeVars xs) =<< es)

-- black magic.  returns a list of list of function names.  When
-- traversed, the list gives in order the set of definitions to check.
-- eg, the image p3 of the paper.pdf could give
-- [[f],[g],[h],[f,g,a],[b],[f,g,h,a,b,c]] 

-- because we check f,g,h first and we do some book-keeping to know
-- what has been or hasn't been checked yet, when checking [f,g,a] we
-- will only check a's contract assuming f's,g's contract hold (it has
-- been checked before)
-- all of this is done by Check.hs, not here.
checkOrder :: Program -> [[Variable]]
checkOrder p = reverse $ map nub topOrder
  where (g,a,b) = graphFromProgram p
        (e,f) = scc $ g
        topOrderAux = map (\(b,as) -> (map a as)) e
        topOrder = map (map (\(_,f,fs) -> f)) topOrderAux
