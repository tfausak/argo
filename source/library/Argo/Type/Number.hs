module Argo.Type.Number
    ( Number(..)
    ) where

data Number
    = Number Integer Integer
    deriving (Eq, Show)
