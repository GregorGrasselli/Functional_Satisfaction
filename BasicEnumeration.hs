{- | Module : BasicEnumeration 

Boolean and variable primitives along with simple functions for
enumerating boolean expressions and testing for tautologies
-}
module BasicEnumeration where

import qualified Data.HashMap as H

bTrue kt _ = kt
bFalse _ kf = kf

-- * Operators
bNot b kt kf = b kf kt

bOr  b1 b2 kt kf = b1 kt (b2 kt kf)
bAnd b1 b2 kt kf = b1 (b2 kt kf) kf

-- * Evaluation

-- | Evaluate a boolean expression without variables.
eval1 b = b True False

-- Environment
type Env a = H.Map String a

-- | Evaluate a boolean expression with variables whose values are
-- given in the environment.
eval b env = b (\_ -> True) (\_ -> False) env

-- | The type of variable only suited for evaluating an expression
-- given an environment.
bVar :: String -> (Env Bool -> a) -> (Env Bool -> a) -> Env Bool -> a
bVar x kt kf env = case H.lookup x env of
                     Just True  -> kt env
                     Just False -> kf env

-- | The type of variable that can be used to enumerate all possible
-- values of a boolean expression.
bVar' :: Monad m => String -> (Env Bool -> m a) -> (Env Bool -> m a) -> Env Bool -> m a
bVar' x kt kf env = case H.lookup x env of
                     Just True  -> kt env
                     Just False -> kf env
                     Nothing    -> kt (H.insert x True env) >>
                                   kf (H.insert x False env)

printTrue  env = putStrLn $ show env ++ " -> True"
printFalse env = putStrLn $ show env ++ " -> False"

-- | Enumerate a boolean expression. bVar' should be used in variable
-- construction.
enum :: Monad m => ((Env a -> m b) -> (Env a -> m b) -> Env a -> m b) -> (Env a -> m b) -> (Env a -> m b) -> m b
enum b kt kf = b kt kf H.empty

-- | This is 'enum' called with
-- continuations that make it print the currrent environment and the
-- value of the expression each time it reaches a value-
enum' b = enum b printTrue printFalse

-- | Checks if the given expression is a tautology.
alwaysTrue b = case testTrue b of
                 Right _ -> True
                 Left  _ -> False

-- | If b is a tautology it returns Right (). Returns Left env
-- otherwise, where env is such an environment that evaluating b
-- w.r.t. env returns False.
testTrue b = enum b (\_ -> Right ()) (\env -> Left env)

-- | Checks if there exists an environment such that the given
-- expression evaluates to true w.r.t. that environment
maybeTrue b = case testTrue $ bNot b of
                Right _ -> False
                Left  _ -> True
