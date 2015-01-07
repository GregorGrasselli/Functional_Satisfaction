module Primeri where

import Graph
import MPlusEnumeration
import BasicEnumeration
import qualified Data.List as L
import qualified Data.HashMap as H (empty, Map, filter, fromList)
import Control.Monad

-- | Makes a graph that looks like a polygon with 2 d diagonals to its closest neighbors.
polygonalGraph :: Int -> Int -> Graph
polygonalGraph s d = L.concatMap f [0..s-1]
    where f i = take (d + 1) $ zip (repeat i) (iterate ((`mod` s) . (+ 1)) ((i + 1) `mod` s))

findGraphSolution :: MonadPlus m => Int -> Int -> Int -> m (H.Map String Bool, Bool)
findGraphSolution n d k = wellColored (polygonalGraph n d) k bVarl (\ e -> return (filterEnv e, True)) (\ _ -> mzero) H.empty
   where filterEnv = H.filter id

findGraphSolutionl ::  Int -> [(H.Map String Bool, Bool)]
findGraphSolutionl n = findGraphSolution n 1 3

findGraphSolutionm :: Int -> Maybe (H.Map String Bool, Bool)
findGraphSolutionm n = findGraphSolution n 1 3

long :: MonadPlus m => (Env Bool -> m a) -> (Env Bool -> m a) -> Env Bool -> m a
long = ((bNot (bVarl "x") `bOr` bVarl "y") `bOr` bVarl "z") `bOr` bVarl "z"

short :: MonadPlus m => (Env Bool -> m a) -> (Env Bool -> m a) -> Env Bool -> m a
short = (bVarl "x" `bOr` (bNot (bVarl "x") `bOr` bVarl "y")) `bOr` bVarl "z"

exampleToEvaluate = H.fromList [("80",False),("81",False),("82",True),("83",False),("93",False),("92",False),("91",False),("90",True),("40",False),("41",True),("42",False),("43",False),("53",False),("52",True),("51",False),("50",False),("62",False),("63",False),("60",True),("61",False),("71",True),("70",False),("73",False),("72",False),("00",True),("01",False),("02",False),("03",False),("13",False),("12",False),("11",True),("10",False),("22",True),("23",False),("20",False),("21",False),("31",False),("30",True),("33",False),("32",False),("112",True),("113",False),("110",False),("111",False),("101",True),("100",False),("103",False),("102",False)]

evalPGraph n d k env = wellColored (polygonalGraph n d) k bVar (\_ -> True) (\_ -> False) env
