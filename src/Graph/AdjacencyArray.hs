module Graph.AdjacencyArray where

import Data.Massiv.Array as A

type AdjacencyMatrixRep a = Array P Ix2 a

adjGraph :: AdjacencyMatrixRep Int
adjGraph =
  fromLists'
    Seq
    [ [0, 1, 0, 0, 1],
      [1, 0, 1, 1, 1],
      [0, 1, 0, 1, 0],
      [0, 1, 1, 0, 1],
      [1, 1, 0, 1, 0]
    ]

adjDigraph :: AdjacencyMatrixRep Int
adjDigraph =
  fromLists'
    Seq
    [ [0, 1, 0, 1, 0, 0],
      [0, 0, 0, 0, 1, 0],
      [0, 0, 0, 0, 1, 1],
      [0, 1, 0, 0, 0, 0],
      [0, 0, 0, 1, 0, 0],
      [0, 0, 0, 0, 0, 1]
    ]

-- λ> propSymmetric adjGraph
-- True
-- λ> propSymmetric adjDigraph
-- False
propSymmetric :: (Eq a, Prim a) => AdjacencyMatrixRep a -> Bool
propSymmetric g = convert (transpose g) == g
