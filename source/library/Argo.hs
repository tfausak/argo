{-# LANGUAGE PatternSynonyms #-}

module Argo
    ( Value.Value
        ( Pattern.Null
        , Pattern.Boolean
        , Pattern.Number
        , Pattern.String
        , Pattern.Array
        , Pattern.Object
        )
    , Type.Array
    , Pair.Pair(Pattern.Pair)
    , Type.Object
    , Encode.encode
    , Decode.decode
    , FromValue.FromValue(fromValue)
    , ToValue.ToValue(toValue)
    , QuasiQuoter.value
    , Result.Result(Failure, Success)
    ) where

import qualified Argo.Class.FromValue as FromValue
import qualified Argo.Class.ToValue as ToValue
import qualified Argo.Decode as Decode
import qualified Argo.Encode as Encode
import qualified Argo.Pattern as Pattern
import qualified Argo.QuasiQuoter as QuasiQuoter
import qualified Argo.Result as Result
import qualified Argo.Type as Type
import qualified Argo.Type.Pair as Pair
import qualified Argo.Type.Value as Value
