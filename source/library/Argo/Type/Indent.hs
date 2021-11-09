module Argo.Type.Indent where

data Indent
    = Spaces Int
    | Tab
    deriving (Eq, Show)
