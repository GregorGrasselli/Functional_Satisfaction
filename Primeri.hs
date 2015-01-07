module Primeri where

import Graph
import MPlusEnumeration
import qualified Data.List as L
import qualified Data.HashMap as H (empty, Map, filter)
import Control.Monad

-- | Makes a graph that looks like a polygon with 2 d diagonals to its closest neighbors.
polygonalGraph :: Int -> Int -> Graph
polygonalGraph s d = L.concatMap f [0..s-1]
    where f i = take (d + 1) $ zip (repeat i) (iterate ((`mod` s) . (+ 1)) ((i + 1) `mod` s))

findGraphSolution :: (Monad m, MonadPlus m) => Int -> Int -> Int -> m (H.Map String Bool, Bool)
findGraphSolution n d k = wellColored (polygonalGraph n d) k bVarl (\ e -> return (e, True)) (\ _ -> mzero) H.empty
--   where filterEnv = H.filter id

findGraphSolutionl ::  Int -> [(H.Map String Bool, Bool)]
findGraphSolutionl n = findGraphSolution n 1 3

findGraphSolutionm :: Int -> Maybe (H.Map String Bool, Bool)
findGraphSolutionm n = findGraphSolution n 1 3
