module Argo.Result where

import qualified Control.Applicative as Applicative

data Result a
    = Failure String
    | Success a
    deriving (Eq, Show)

instance Functor Result where
    fmap f r = case r of
        Failure e -> Failure e
        Success x -> Success $ f x

instance Applicative Result where
    pure = Success
    rf <*> rx = case (rf, rx) of
        (Failure e, _) -> Failure e
        (_, Failure e) -> Failure e
        (Success f, Success x) -> Success $ f x

instance Monad Result where
    r >>= f = case r of
        Failure e -> Failure e
        Success x -> f x

instance MonadFail Result where
    fail = Failure

instance Applicative.Alternative Result where
    empty = fail "empty"
    rx <|> ry = case rx of
        Failure _ -> ry
        Success _ -> rx
