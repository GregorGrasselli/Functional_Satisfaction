module Ints where

import qualified Data.Set as S
import Data.List (intercalate)
import Prelude hiding (lookup)
import BasicEnumeration (Env)
import Data.HashMap as H (lookup)

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
