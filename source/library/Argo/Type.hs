module Argo.Type where

import qualified Argo.Type.Pair as Pair
import qualified Argo.Type.String as String
import qualified Argo.Type.Value as Value
import qualified Data.Array

type Array = Data.Array.Array Int Value.Value

type Object = Data.Array.Array Int (Pair.Pair String.String Value.Value)
