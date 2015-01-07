module Graph where

import qualified Data.List as L

import BasicEnumeration

-- | A graph is given as a list of edges. Vertex indices start with 0!
type Graph = [(Int, Int)]

condA g k bvar = foldr f bTrue $ take (size g) [0..]
    where f e a = bAnd a $ foldr bOr bFalse (map bvar (map ((show e) ++) $ map show [0..k]))

condB g k bvar = foldr f bTrue $ take (size g) [0..]
    where f e a = bAnd a $ foldr bAnd bTrue ( map (\(x, y) -> bNot $ bAnd (bvar x) (bvar y)) [(show e ++ show b1, show e ++ show b2) | b1 <-  [0..k], b2 <- [0..k], b1 /= b2])

condC g k bvar = foldr f bTrue g
    where f (j, t) a = bAnd a $ bNot $ foldr bOr bFalse (map (\b -> bAnd (bvar $ show j ++ show b) (bvar $ show t ++ show b)) [0..k])

wellColored g k bvar = condA g k bvar `bAnd` condB g k bvar `bAnd` condC g k bvar

size :: Graph -> Int
size = length . L.nub . L.sort . L.concatMap (\(a,b) -> [a, b])
