module Graph where

import qualified Data.List as L

import BasicEnumeration

-- every element is a list of neighbours whose indices are greater then the current index
type Graph = [(Int, Int)]

--bOr :: (t2 -> t1 -> t) -> (t2 -> t3 -> t1) -> t2 -> t3 -> t
-- --conditionA :: Graph -> Int -> t2 -> t3 -> t
-- conditionA g k bvar kt kf = foldl f bTrue g
--     where f g acc = bAnd acc $ foldl bOr bFalse (map (\x -> bvar lst

condA g k bvar = L.foldl' f bTrue $ take (size g) [0..]
    where f a e = bAnd a $ L.foldl' bOr bFalse (map bvar (map ((show e) ++) $ map show [0..k]))

condB g k bvar = L.foldl' f bTrue $ take (size g) [0..]
    where f a e = bAnd a $ L.foldl' bAnd bTrue ( map (\(x, y) -> bNot $ bAnd (bvar x) (bvar y)) [(show e ++ show b1, show e ++ show b2) | b1 <-  [0..k], b2 <- [0..k], b1 /= b2])

condC g k bvar = L.foldl' f bTrue g
    where f a (j, t) = bAnd a $ bNot $ L.foldl' bOr bFalse (map (\b -> bAnd (bvar $ show j ++ show b) (bvar $ show t ++ show b)) [0..k])

wellColored g k bvar = condA g k bvar `bAnd` condB g k bvar `bAnd` condC g k bvar

size :: Graph -> Int
size = length . L.nub . L.sort . L.concatMap (\(a,b) -> [a, b])
