module Analysis where

-- http://hackage.haskell.org/packages/archive/containers/0.4.1.0/doc/html/Data-Graph.html#v:graphFromEdges
import Data.Graph (graphFromEdges,Graph,Vertex,SCC(..))
-- http://hackage.haskell.org/packages/archive/GraphSCC/1.0.2/doc/html/Data-Graph-SCC.html
import Data.Graph.SCC (sccGraph)
import Data.Maybe (fromJust)
import Data.List (nub)
import qualified Data.Map as M

import Haskell
import Parser

{- Some simple graph examples to try: -}
--
-- 0   3
--  \ /|
--   2 |
--  / \|
-- 1   4
--
-- directed left to right, with double edges between 3 and 4.
edges = [(0,0,[2]),(1,1,[2]),(2,2,[3,4]),(3,3,[4]),(4,4,[3])]
fstOf3 (x,_,_) = x
g = fstOf3 $ graphFromEdges $ edges
-- Quotient by SCC relation.
qEdges = sccGraph g
q = fstOf3 $ graphFromEdges $ qEdges
-- SCCs and their dependencies.

instance Show a => Show (SCC a) where
  show (AcyclicSCC v) = "A "++show v
  show (CyclicSCC vs) = "C "++show vs

scc = undefined

-- 1. Build a graph G of dependencies, with 'Name's for nodes, where
-- 'n1' points to 'n2' if 'n1' depends on 'n2', which in turn happens
-- iff 'n1's definition mentions 'n2', 'n1's contract mentions 'n2',
-- or 'n2' is a 'DataType' and 'n1' mentions one of it's term
-- constructors.
--
-- 2. Compute the quotient graph Q of strongly connected components
-- (SCCs) of G, and topologically sort Q.
--
-- 3. Return the SCC's of Q, along with there transitive dependencies,
-- in topological order.  Here "topological order" means dependencies
-- come before things that depend on them.
mkGAdjacency defs = [(f,f, map substType ds) | (f,ds) <- map adj funDefs]
 where
  funDefs = [d | Def d <- defs]
  -- Term constructor types.
  types = [(c,t) | DataType (Data t cs) <- defs, (c,_,_) <- cs]
  -- Substitute a term con for its type.
  substType :: Name -> Name
  substType c = case lookup c types of
                  Nothing -> c
                  Just t  -> t
  -- Adjacency of a single function, but with term constructors still
  -- to be substituted.
  adj (Let f xs e)         = (f, freeVars xs e ++ allContFreeVars f)
  adj (LetCase f xs e pes) = (f, freeVars xs e ++ allContFreeVars f ++
                                       concat [freeVars (xs++ysi) ei
                                              | ((_,ysi),ei) <- pes])
  -- Free vars for all contracts in program for 'f'.
  allContFreeVars f = contFreeVars =<< [c | ContSat (Satisfies f' c) <- defs, f' == f]

-- TODO:
--
-- 1. make sure all data types have nodes, since the Data.Graph
-- ignores nodes that in adjs that don't have an adj list themselves?
--
-- 2. consider using (DefGeneral,Name,[Name]) instead of
-- (Name,Name,[Name]) for the Graph.  see what the code in Check.hs
-- uses us for first.  In any case, we have to look up all the
-- contracts, since we don't include them in the graph (we'd lose
-- uniqueness of keys).

graphFromProgram = graphFromEdges . mkGAdjacency

-- returns the variables used in a contract
contFreeVars c = go [] c where
  go xs (Arr mx c1 c2) = go xs' c1 ++ go xs' c2 where xs' = maybe xs (:xs) mx
  go xs (Pred x e)    = freeVars (x:xs) e
  go xs (And c1 c2)   = go xs c1 ++ go xs c2
  go xs (Or c1 c2)    = go xs c1 ++ go xs c2
  go xs CF            = []
  go xs Any           = []

-- | Return the list of free variables in an expression.
--
-- 'xs' is a list of bound variables.

-- NB: this function returns constructor names as well, although these
-- should never be considered free.  The old code did this, so I
-- preserved this behavior when I updated it, but I did not check that
-- it was necessary --- NC. UPDATE: yes, we want the constructor
-- names, so we can deduce what types a term depends on.
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
