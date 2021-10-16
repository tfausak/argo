{-# LANGUAGE PatternSynonyms #-}

module Argo
    ( Type.Value
    , Type.Array
    , Type.Pair
    , Type.Object
    , pattern Pattern.Null
    , pattern Pattern.Boolean
    , pattern Pattern.Number
    , pattern Pattern.String
    , pattern Pattern.Array
    , pattern Pattern.Object
    , pattern Pattern.Pair
    , Encode.encode
    , Decode.decode
    , FromValue.FromValue(fromValue)
    , ToValue.ToValue(toValue)
    ) where

import qualified Argo.Class.FromValue as FromValue
import qualified Argo.Class.ToValue as ToValue
import qualified Argo.Decode as Decode
import qualified Argo.Encode as Encode
import qualified Argo.Pattern as Pattern
import qualified Argo.Type as Type
