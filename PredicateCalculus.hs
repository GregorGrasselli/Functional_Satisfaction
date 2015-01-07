module PredicateCalculus where

import Ints
import qualified Data.Set as S
import qualified Data.HashMap as H (insert, empty, lookup)
import Control.Monad (mplus, mzero)
import BasicEnumeration
import MPlusEnumeration
import Prelude hiding (lookup)

-- | Find the constraints on a variable enforced by the given
-- environment.
lookup :: String -> Env Ints -> Ints
lookup x u = case H.lookup x u of
               Just s  -> s
               Nothing -> universe

bEquals x i = bMake x (singleton i)

bBounded x l u p = bMake x (Finite $ S.fromList $ filter (runPredicate p) [l..u])

bMake x vp kt kf env = f vt kt `mplus` f vf kf
    where
      f s k = if not $ emptyp s then k $ H.insert x s env else mzero
      vx = lookup x env
      vt = intersection vx vp
      vf = intersection vx $ complement vp

-- This is only here to make it an instance of show, so that 
newtype Predicate = Predicate {runPredicate :: Int -> Bool}

instance Show Predicate where
    show _ = "pred"

data Cond = Equals String Int | Or [Cond] | And [Cond] | Bounded String Int Int Predicate
            deriving Show


compileCond (Equals s i) kt kf       = bEquals s i kt kf
compileCond (Or (c:cs))  kt kf       = bOr (compileCond c) (compileCond $ Or cs) kt kf
compileCond (Or [])      _  kf       = kf
compileCond (And (c:cs)) kt kf       = bAnd (compileCond c) (compileCond $ And cs) kt kf
compileCond (And [])     kt _        = kt
compileCond (Bounded x l u p)  kt kf = bBounded x l u p kt kf

-- * Program equivalence checker
data Stmt = If Cond Stmt Stmt | Decision Int deriving Show

--compileStmt :: Stmt -> (Int -> Env Ints -> [a]) -> Env Ints -> [a]
compileStmt (If c st sf) k = compileCond c (compileStmt st k) (compileStmt sf k)
compileStmt (Decision j) k = k j

-- could play arround some more with the representation; this looks ugly
enumStmt st = mapM_ (putStrLn . show) $ compileStmt st (\i env -> [(show env, i)]) H.empty

-- prints a list of differences can be modified to do other things
equivStmt s1 s2 = compileStmt s1 (\i e -> compileStmt s2 (\i' e' -> if i /= i' then [(i, i', e, e')] else []) e) H.empty
