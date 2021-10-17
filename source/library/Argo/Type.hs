module Argo.Type where

import qualified Argo.Type.Pair as Pair
import qualified Argo.Type.String as String
import qualified Argo.Type.Value as Value
import qualified Data.Array

type Value = Value.Value

type Array = Data.Array.Array Int Value

type Pair = Pair.Pair String.String Value

type Object = Data.Array.Array Int Pair
