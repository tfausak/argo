module Argo.Type.Permission where

data Permission
    = Allow
    | Forbid
    deriving (Eq, Show)

toBool :: Permission -> Bool
toBool x = case x of
    Allow -> True
    Forbid -> False
