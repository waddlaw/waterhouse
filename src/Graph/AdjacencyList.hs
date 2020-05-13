module Graph.AdjacencyList where

import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Graph.Common
import Graph.Directed
import Graph.UnDirected

-- type AdjacencyListRep a = [(a, [a])]
type Edge = (Vertex, Vertex)

type AdjacencyListRep a = Map a (Set a)

adjGraph :: AdjacencyListRep Int
adjGraph =
  Map.fromList
    [ (1, Set.fromList [2, 5]),
      (2, Set.fromList [1, 3, 4, 5]),
      (3, Set.fromList [2, 4]),
      (4, Set.fromList [2, 3, 5]),
      (5, Set.fromList [1, 2, 4])
    ]

adjDigraph :: AdjacencyListRep Int
adjDigraph =
  Map.fromList
    [ (1, Set.fromList [2, 4]),
      (2, Set.fromList [5]),
      (3, Set.fromList [5, 6]),
      (4, Set.fromList [2]),
      (5, Set.fromList [4]),
      (6, Set.fromList [6])
    ]

-- λ> graphToListRep graph == adjGraph
-- True
graphToListRep :: Graph -> AdjacencyListRep Int
graphToListRep g@(vs, es)
  | isGraph g =
    let idxs = Set.toAscList vs
        es' = Set.toList es
        ts = [(idx, Set.fromList [Set.elemAt 0 (Set.delete idx e) | e <- es', idx `Set.member` e]) | idx <- idxs]
     in Map.fromList ts
  | otherwise = error "Not Graph"

-- λ> digraphToListRep digraph == adjDigraph
-- True
digraphToListRep :: DiGraph -> AdjacencyListRep Int
digraphToListRep g@(vs, es)
  | isDiGraph g =
    let idxs = Set.toAscList vs
        es' = Set.toList es
        ts = [(idx, Set.fromList [j | (i, j) <- es', idx == i]) | idx <- idxs]
     in Map.fromList ts
  | otherwise = error "Not Digraph"

-- λ> adjSizeE adjGraph
-- 14
-- λ> adjSizeE adjDigraph
-- 8
adjSizeE :: AdjacencyListRep a -> Int
adjSizeE = sum . map Set.size . Map.elems

-- λ> propGraphLength graph
-- True
propGraphLength :: Graph -> Bool
propGraphLength g = 2 * sizeE g == adjSizeE (graphToListRep g)

-- λ> propDiGraphLength digraph
-- True
propDiGraphLength :: DiGraph -> Bool
propDiGraphLength g = sizeE g == adjSizeE (digraphToListRep g)

-- λ> existEdge adjGraph (1,2)
-- True
-- λ> existEdge adjGraph (1,3)
-- False
-- λ> existEdge adjGraph (2,1)
-- False
-- λ> existEdge adjDigraph (1,2)
-- True
-- λ> existEdge adjDigraph (1,3)
-- False
-- λ> existEdge adjDigraph (2,1)
-- Flase
existEdge :: AdjacencyListRep Int -> Edge -> Bool
existEdge g (u, v) =
  isJust $ do
    ts <- Map.lookup u g
    v `Set.lookupIndex` ts
