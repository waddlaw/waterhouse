module Graph.UnDirected (Graph, isGraph) where

import Data.Set (Set)
import qualified Data.Set as Set
import Graph.Common

type Edge = Set Vertex

type Graph = (Set Vertex, Set Edge)

graph :: Graph
graph = (vs, es)
  where
    vs = Set.fromList [1 .. 5]
    es =
      Set.fromList
        [ Set.fromList [1, 2],
          Set.fromList [1, 5],
          Set.fromList [2, 3],
          Set.fromList [2, 4],
          Set.fromList [2, 5],
          Set.fromList [3, 4],
          Set.fromList [4, 5]
        ]

badGraph1 :: Graph
badGraph1 = (vs, es)
  where
    vs = Set.fromList [1]
    es = Set.fromList [Set.fromList [1, 2]]

badGraph2 :: Graph
badGraph2 = (vs, es)
  where
    vs = Set.fromList [1]
    es = Set.fromList [Set.fromList [1, 1]]

badGraph3 :: Graph
badGraph3 = (vs, es)
  where
    vs = Set.fromList [1]
    es = Set.fromList [Set.fromList [1, 6], Set.fromList [1, 2]]

-- λ> isGraph graph
-- True
isGraph :: Graph -> Bool
isGraph (vs, es) = check1 && check2
  where
    -- Check that there is no side to yourself.
    check1 = Set.foldr (\x acc -> Set.size x == 2 && acc) True $ es
    -- Checks whether all the origins of a given set of edges are contained in the cartesian set of vertices.
    vs' = Set.cartesianProduct vs vs
    check2 =
      Set.foldr
        ( \x acc ->
            let [a, b] = Set.toList x
             in and
                  [ (a, b) `Set.member` vs',
                    (b, a) `Set.member` vs',
                    acc
                  ]
        )
        True
        es
