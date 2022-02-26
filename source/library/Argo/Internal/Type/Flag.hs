module Argo.Internal.Type.Flag where

data Flag
    = Help
    | Spaces String
    | Tab
    | Version
    deriving (Eq, Show)
