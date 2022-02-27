module Argo.Internal.Type.Indent where

data Indent
    = Spaces Int
    | Tab
    deriving (Eq, Show)
