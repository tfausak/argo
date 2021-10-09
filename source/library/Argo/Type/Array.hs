module Argo.Type.Array where

import qualified Data.Array as Array

newtype Array a
    = Array (Array.Array Int a)
    deriving (Eq, Show)
