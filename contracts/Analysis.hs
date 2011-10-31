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
graphFromProgram p = graphFromEdges [go d | d@(Def _) <- p]
  where go (Def (Let f xs e)) = (f,f,freeVars (xs++dataV) e ++ varsInCont p f)
        go (Def (LetCase f xs e pes)) = (f,f,varsInCont p f ++ (concatMap (\((_,ys),e) -> freeVars (xs++dataV++ys) e) $ ((undefined,[]),e):pes))
--        go (ContSat (Satisfies f c))  = (f,f,contVars f dataV c)
        dataV = dataVars =<< [d | d@(DataType _) <- p]

-- finds the contract for f in p and returns the variables in it
-- Consider this example: 
-- f x = x
-- f ::: {x: eq x 0} -> {y: eq y 0}
-- f doesn't use any function so it has nothing pointing to it in the graph
-- but still, we need to include eq's definition in the theory!
varsInCont p f = contVars f [] =<< [c | ContSat (Satisfies x c) <- p, x == f]

-- returns the variables used in a contract
contVars f xs (Arr x c1 c2) = contVars f xs c1 ++ contVars f xs c2
contVars f xs (Pred x e)     = freeVars (x:xs) e
contVars f xs (And c1 c2)    = contVars f xs c1 ++ contVars f xs c2
contVars f xs (Or c1 c2)     = contVars f xs c1 ++ contVars f xs c2
contVars f xs CF             = []
contVars f xs Any            = []
        
dataVars :: DefGeneral -> [Variable]
dataVars (DataType (Data _ vacs)) = map fst3 vacs
  where fst3 (a,_,_) = a

-- | Return the list of free variables in an expression.
--
-- 'xs' is a list of bound variables.

-- XXX, BUG???: this function returns constructor names as well,
-- although these should never be considered free.  The old code did
-- this, so I preserved this behavior when I updated it, but I did not
-- check that it was necessary --- NC.
freeVars :: [Name] -> Expression -> [Name]
freeVars xs (Named v) = if (getName v) `elem` xs then [] else [getName v]
freeVars xs (e1 :@: e2) = freeVars xs e1 ++ freeVars xs e2
freeVars xs (FullApp g es) = (getName g) : ((freeVars xs) =<< es)

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
