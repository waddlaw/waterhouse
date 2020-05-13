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
