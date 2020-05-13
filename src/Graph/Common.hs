module Graph.Common where

import qualified Data.Set as Set

type Vertex = Int

-- sizeV, sizeE :: DiGraph -> Int
sizeV = Set.size . fst

sizeE = Set.size . snd

data RepType = Sparse | Dense
  deriving (Show)

-- checkRepType :: DiGraph -> RepType
checkRepType g = if (e / v ^ 2) < threshold then Sparse else Dense
  where
    v = fromIntegral $ sizeV g
    e = fromIntegral $ sizeE g
    threshold = 0.1
