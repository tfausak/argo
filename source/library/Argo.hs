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
    , Pattern.Array
    , Name.Name(Pattern.Name)
    , Pattern.Member
    , pattern Pattern.Member
    , Pattern.Object
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
import qualified Argo.Type.Name as Name
import qualified Argo.Type.Value as Value
