module Argo.Type.Pair
    ( Pair(..)
    ) where

newtype Pair k v
    = Pair (k, v)
    deriving (Eq, Show)
