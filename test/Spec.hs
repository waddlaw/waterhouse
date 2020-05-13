module Spec (main) where

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Ingredients.Rerun
import Test.Tasty.QuickCheck as QC
import Data.Massiv.Array as A

import Graph.AdjacencyArray

main :: IO ()
main = defaultMainWithRerun tests

tests :: TestTree
tests = testGroup "Tests" [unitTests] -- [properties, unitTests]

-- properties :: TestTree
-- properties = testGroup "Properties" [qcProps]

unitTests =
  testGroup
    "Unit tests"
    [ testCase "List comparison (different length)" $
        [1, 2, 3] `compare` [1, 2] @?= GT,
      -- the following test does not hold
      testCase "List comparison (same length)" $
        [1, 2, 3] `compare` [1, 2, 2] @?= LT
    ]

-- qcProps :: TestTree
-- qcProps =
--   testGroup
--     "(checked by QuickCheck)"
--     [ QC.testProperty "undirected graph: convert . transpose == id" $
--         prop_symmetric
--     ]

-- λ> propSymmetric adjGraph
-- True
-- λ> propSymmetric adjDigraph
-- False
-- prop_symmetric :: (Eq a, Prim a) => AdjacencyMatrixRep a -> Bool
-- prop_symmetric g = A.toLists2 (convert (transpose g)) == A.toLists2 g

-- λ> propGraphLength graph
-- True
-- propGraphLength :: Graph -> Bool
-- propGraphLength g = 2 * sizeE g == adjSizeE (graphToListRep g)

-- λ> propDiGraphLength digraph
-- True
-- propDiGraphLength :: DiGraph -> Bool
-- propDiGraphLength g = sizeE g == adjSizeE (digraphToListRep g)