import qualified Data.HashMap as H

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
bVarl :: String -> (Env Bool -> [String]) -> (Env Bool -> [String]) -> Env Bool -> [String]
bVarl x kt kf env = case H.lookup x env of
                      Just True -> kt env
                      Just False ->  kf env
                      Nothing -> kt (H.insert x True env) ++
                                 kf (H.insert x False env)

enuml b = mapM_ putStrLn $ b (\e -> [show e ++ " -> True"]) (\e -> [show e ++ " -> False"]) H.empty

alwaysTrue b = and $ b (\_ -> True) (\_ -> False) H.empty
maybeTrue  b = or  $ b (\_ -> True) (\_ -> False) H.empty

-- |* Monadic predicate calculus

data Cond = Equals String Int | Or [Cond] | And [Cond]
