module Argo.Type.Object where

import qualified Argo.Type.Pair as Pair
import qualified Argo.Type.String as String
import qualified Data.Array as Array

newtype Object a
    = Object (Array.Array Int (Pair.Pair String.String a))
    deriving (Eq, Show)
