-- Compute the defs that generate the theory to check.
--
-- 1. Build a graph G of dependencies, with function and datatype defs
-- for nodes, and function and datype names for keys.  Here 'd1'
-- points to 'd2' if 'd1' depends on 'd2', i.e. iff 'd1's definition
-- mentions 'd2', 'd1's contract mentions 'd2', 'd1' has a predicate
-- contract and 'd2' is 'Bool', or 'd2' is a 'DataType' and 'd1'
-- mentions one of it's term constructors.
--
-- NB: the G does not include contracts. The main reason is that NC
-- couldn't think of a good way to get unique keys for them, since we
-- already key function defs by the function name.  So, the theory
-- generation code has to look up the contracts for the functions it
-- processes.
--
-- 2. Compute the quotient graph Q of strongly connected components
-- (SCCs) of G. Return the defs in the SCC's of Q, along with there
-- transitive dependencies, in topological order.  Here "topological
-- order" means dependencies come before things that depend on them.
module Analysis where

-- http://hackage.haskell.org/packages/archive/containers/0.4.1.0/doc/html/Data-Graph.html#v:graphFromEdges
import qualified Data.Graph as G
-- http://hackage.haskell.org/packages/archive/GraphSCC/1.0.2/doc/html/Data-Graph-SCC.html
import Data.Graph.SCC (sccGraph)
import Data.Maybe (fromJust,catMaybes)
import Data.List (nub,delete)

import Haskell

fstOf3 (x,_,_) = x

{- Graph example to help understand Data.Graph functions. Try in GHCi:
--
-- 0   3
--  \ /|
--   2 |
--  / \|
-- 1   4
--
-- directed left to right, with double edges between 3 and 4.
-}
edges = [("0",0,[2]),("1",1,[2]),("2",2,[3,4]),("3",3,[4]),("4",4,[3])]
g = fstOf3 $ G.graphFromEdges $ edges
-- Quotient by SCC relation.
qEdges = sccGraph g
q = fstOf3 $ G.graphFromEdges $ qEdges
-- SCCs and their dependencies.

instance Show a => Show (G.SCC a) where
  show (G.AcyclicSCC v) = "A "++show v
  show (G.CyclicSCC vs) = "C "++show vs
{- End examples -}

-- (1)
adjacencies defs = funAdjs ++ typeAdjs
 where
  -- Adjacency of a single function, but with term constructors still
  -- to be substituted.
  adj d@(Def (Let f xs ce)) = (d,f,deps) where
    -- Contract dependencies.
    cDeps = freeVarsC =<<
              [c | ContSat (Satisfies f' c) <- defs, f' == f]
    -- Pattern dependencies.
    ceDeps = freeVarsCE xs ce
    deps = cDeps ++ ceDeps

  funAdjsWithCons = map adj [d | d@(Def _) <- defs]
  -- Term constructor types.
  con2Type = [(c,d) | d@(DataType (Data _ cs)) <- defs, (c,_,_) <- cs]
  -- Replace term constructors with their types in dependencies.
  funAdjs = map (\(d,f,ds) -> (d,f,map substType ds)) funAdjsWithCons where
    -- Substitute a term con for its type.
    substType :: Name -> Name
    substType c = case lookup c con2Type of
                    Nothing -> c
                    Just (DataType (Data t _)) -> t

  -- Add empty adjs for each type appearing in dependencies:
  -- Data.Graph.graphFromEdges discards dependencies w/o an adjacency
  -- list :P (Wouldn't it be better to just assume they have no out
  -- edges?)
  typeAdjs = [(d,t,[]) | d@(DataType (Data t _)) <- typeDeps] where
    -- All dependencies, still with term constructors.
    deps = nub $ concat $ [ds | (_,_,ds) <- funAdjsWithCons]
    -- Types of all term constructors in deps.
    typeDeps :: [TopLevelStatement]
    typeDeps = nub $ catMaybes $ map (`lookup` con2Type) deps

-- (2)
--
-- Compute pairs of '(checks, depends)', in topological order of the
-- 'checks's, where 'depends' are the dependencies of the 'checks'.
-- The 'checks' are a strongly connected component in the dependency
-- graph.
orderedChecks :: Program -> [([TopLevelStatement],[TopLevelStatement])]
orderedChecks defs = checkDeps where
  -- Dependency graph 'g'.
  gAdjs = adjacencies defs
  (g,gVertex2Adj,_) = G.graphFromEdges gAdjs

  -- Compute SCC quotient graph of 'g'.
  qAdjs :: [(G.SCC G.Vertex, Int, [Int])]
  -- Reverse here to get topo order: sccGraph returns adjs in reverse
  -- topo order.  The 'Vertex's here are from the 'g', the 'Int' keys
  -- are new.
  qAdjs = reverse . sccGraph $ g
  (q,qVertex2Adj,qKey2MaybeVertex) = G.graphFromEdges qAdjs

  checkDeps :: [([TopLevelStatement],[TopLevelStatement])]
  checkDeps = [ (sccVertex2Defs v, sccVertex2Defs =<< ds)
              | (v,ds) <- sccCheckDeps
              ]
   where
    -- The '(checks,deps)' pairs of SCC vertices.
    sccCheckDeps = [ (v,delete v (G.reachable q v)) -- 'delete' in case of self-edge.
                   | (_,k,_) <-qAdjs,  Just v <- [qKey2MaybeVertex k]
                   ]
    -- Recover the definitions from an SCC vertex.  There are two levels
    -- of indirection here, because each graph abtracts nodes via keys.
    -- Here we follow the keys backwards to their nodes.
    sccVertex2Defs :: G.Vertex -> [TopLevelStatement]
    sccVertex2Defs = map (fstOf3 . gVertex2Adj) -- recover g nodes
                   . G.flattenSCC               -- get list from SCC
                   . fstOf3 . qVertex2Adj       -- recover q nodes

-- returns the variables used in a contract
freeVarsC c = go [] c where
  go xs (Arr mx c1 c2) = go xs' c1 ++ go xs' c2 where xs' = maybe xs (:xs) mx
  go xs (Pred x e)    = "Bool":freeVars (x:xs) e -- All preds depend on 'Bool'
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


freeVarsCE bvs (Base e) = freeVars bvs e
freeVarsCE bvs (Case e pces) =
  freeVars bvs e ++ concat [ c : freeVarsCE (vs++bvs) ce
                           | ((c,vs),ce) <- pces ]