module PredicateCalculus where

import Ints
import Data.HashMap as H (insert, empty)
import Control.Monad (mplus, mzero)
import BasicEnumeration
import Prelude hiding (lookup)

bEquals x i kt kf env = (if member i vx then kt $ H.insert x (singleton i) env
                         else mzero) `mplus`
                        (if not $ emptyp vf then kf $ H.insert x vf env
                         else mzero)
                            where
                              vx = lookup x env
                              vf = intersection vx $ complement $ singleton i

data Cond = Equals String Int | Or [Cond] | And [Cond]
          deriving Show

-- compileCond :: Cond -> (Env Ints -> [a]) -> (Env Ints -> [a]) -> Env Ints -> [a]
-- that type makes the whole thing run on lists
compileCond (Equals s i) kt kf = bEquals s i kt kf
compileCond (Or (c:cs))  kt kf = bOr (compileCond c) (compileCond $ Or cs) kt kf
compileCond (Or [])      _  kf = kf
compileCond (And (c:cs)) kt kf = bAnd (compileCond c) (compileCond $ And cs) kt kf
compileCond (And [])     kt _  = kt

-- |* Program equivalence checker
data Stmt = If Cond Stmt Stmt | Decision Int deriving Show

--compileStmt :: Stmt -> (Int -> Env Ints -> [a]) -> Env Ints -> [a]
compileStmt (If c st sf) k = compileCond c (compileStmt st k) (compileStmt sf k)
compileStmt (Decision j) k = k j

-- could play arround some more with the representation; this looks ugly
enumStmt st = mapM_ (putStrLn . show) $ compileStmt st (\i env -> [(show env, i)]) H.empty

-- prints a list of differences can be modified to do other things
equivStmt s1 s2 = compileStmt s1 (\i e -> compileStmt s2 (\i' e' -> if i /= i' then [(i, i', e, e')] else []) e) H.empty
