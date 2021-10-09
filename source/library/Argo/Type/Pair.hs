module Argo.Type.Pair where

newtype Pair k v
    = Pair (k, v)
    deriving (Eq, Show)
