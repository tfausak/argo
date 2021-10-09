module Argo.Type.Boolean
    ( Boolean(..)
    ) where

newtype Boolean
    = Boolean Bool
    deriving (Eq, Show)
