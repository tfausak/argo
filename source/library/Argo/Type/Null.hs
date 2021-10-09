module Argo.Type.Null
    ( Null(..)
    ) where

newtype Null
    = Null ()
    deriving (Eq, Show)
