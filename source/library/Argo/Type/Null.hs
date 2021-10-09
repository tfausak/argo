module Argo.Type.Null where

newtype Null
    = Null ()
    deriving (Eq, Show)
