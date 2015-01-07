module MPlusEnumeration where

import Data.HashMap as H
import Control.Monad
import BasicEnumeration (Env)

-- * enumeration and properties the lazy way (last paragraph of
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
