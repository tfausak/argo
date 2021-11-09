module Argo.Type.Permission where

data Permission
    = Allow
    | Forbid
    deriving (Eq, Show)
