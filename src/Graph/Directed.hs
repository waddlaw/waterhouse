module Graph.Directed (DiGraph, isDiGraph, digraph) where

import Data.Set (Set)
import qualified Data.Set as Set
import Graph.Common

type DiEdge = (Vertex, Vertex)

type DiGraph = (Set Vertex, Set DiEdge)

digraph :: DiGraph
digraph = (vs, es)
  where
    vs = Set.fromList [1 .. 6]
    es =
      Set.fromList
        [ (1, 2),
          (1, 4),
          (2, 5),
          (3, 5),
          (3, 6),
          (4, 2),
          (5, 4),
          (6, 6)
        ]

badDigraph :: DiGraph
badDigraph = (vs, es)
  where
    vs = Set.fromList [1]
    es = Set.fromList [(1, 5), (1, 1)]

-- λ> isDiGraph digraph
-- True
isDiGraph :: DiGraph -> Bool
isDiGraph (vs, es) = es `Set.isSubsetOf` vs'
  where
    vs' = Set.cartesianProduct vs vs
