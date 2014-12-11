module Prepis where
import qualified Data.HashMap as H
import qualified Data.Set as S
import Data.List (intercalate)
import Prelude hiding (lookup)
import Control.Monad.Plus

bTrue kt _ = kt
bFalse _ kf = kf

-- operators
bNot b kt kf = b kf kt

bOr  b1 b2 kt kf = b1 kt (b2 kt kf)
bAnd b1 b2 kt kf = b1 (b2 kt kf) kf

-- evaluation
eval1 b = b True False

-- Environment
type Env a = H.Map String a

eval b env = b (\_ -> True) (\_ -> False) env

bVar :: String -> (Env Bool -> a) -> (Env Bool -> a) -> Env Bool -> a
bVar x kt kf env = case H.lookup x env of
                     Just True  -> kt env
                     Just False -> kf env

-- enumeration
bVar' :: Monad m => String -> (Env Bool -> m a) -> (Env Bool -> m a) -> Env Bool -> m a
bVar' x kt kf env = case H.lookup x env of
                     Just True  -> kt env
                     Just False -> kf env
                     Nothing    -> kt (H.insert x True env) >>
                                   kf (H.insert x False env)

printTrue  env = putStrLn $ show env ++ " -> True"
printFalse env = putStrLn $ show env ++ " -> False"

enum :: Monad m => ((Env a -> m b) -> (Env a -> m b) -> Env a -> m b) -> (Env a -> m b) -> (Env a -> m b) -> m b
enum b kt kf = b kt kf H.empty

-- the enum from the article
enum' b = enum b printTrue printFalse

-- property finding
alwaysTrue b = case testTrue b of
                 Right _ -> True
                 Left  _ -> False

-- | If b is a tautology it returns Right (). Returns Left env
-- otherwise, where env is such an environment that evaluating b
-- w.r.t. env returns False.
testTrue b = enum b (\_ -> Right ()) (\env -> Left env)

maybeTrue b = case testTrue $ bNot b of
                Right _ -> False
                Left  _ -> True

-- |* enumeration and properties the lazy way (last paragraph of
-- section 3.4) all functions here have an added l postfix
bVarl :: MonadPlus m => String -> (Env Bool -> m a) -> (Env Bool -> m a) -> Env Bool -> m a
bVarl x kt kf env = case H.lookup x env of
                      Just True -> kt env
                      Just False ->  kf env
                      Nothing -> kt (H.insert x True env) `mplus`
                                 kf (H.insert x False env)

enuml b = mapM_ putStrLn $ b (\e -> [show e ++ " -> True"]) (\e -> [show e ++ " -> False"]) H.empty

-- find an instance of an environment for which the given expression
-- is True or false respectively
findTrue b = b (\e -> Just e) (\_ -> Nothing) H.empty
findFalse b = b (\_ -> Nothing) (\e -> Just e) H.empty

alwaysTruel b = and $ b (\_ -> [True]) (\_ -> [False]) H.empty
maybeTruel  b = or  $ b (\_ -> [True]) (\_ -> [False]) H.empty

-- |* Monadic predicate calculus

-- |* Ints; should be moved to other file
data Ints = Finite (S.Set Int) | CoFinite (S.Set Int)

instance Show Ints where
    show (Finite a)   = "{" ++ intercalate ", " (map show (S.toList a)) ++ "}"
    show (CoFinite b) = "Z \\ " ++ show (Finite b)

emptyp :: Ints -> Bool
emptyp (Finite x) | S.null x = True
emptyp _                     = False

intersection (Finite a)   (Finite b)   = Finite (S.intersection a b)
intersection (Finite a)   (CoFinite b) = Finite (S.difference a b)
intersection (CoFinite a) (CoFinite b) = CoFinite (S.union a b)
intersection (CoFinite a) (Finite b)   = Finite (S.difference b a)

complement (Finite a)   = CoFinite a
complement (CoFinite a) = Finite a

union a b = complement $ intersection (complement a) (complement b)

singleton i = Finite (S.singleton i)

empty = Finite (S.empty)
universe = complement empty

lookup :: String -> Env Ints -> Ints
lookup x u = case H.lookup x u of
               Just s  -> s
               Nothing -> universe

member i (Finite a)   = S.member i a
member i (CoFinite a) = S.notMember i a

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
